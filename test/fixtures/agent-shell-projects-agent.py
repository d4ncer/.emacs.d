"""Deterministic ACP peer for exercising the real Emacs agent-shell bridge."""

import json
import os
from pathlib import Path
import random
import re
import subprocess
import sys
import time
import uuid


def emit(message):
    print(json.dumps({"jsonrpc": "2.0", **message}), flush=True)


def operation(bundle, inbox, op, **fields):
    request = bundle["request"]
    value = {
        "id": uuid.uuid4().hex,
        "project": bundle["project"]["id"],
        "agent": request["agent"],
        "op": op,
        **fields,
    }
    inbox.mkdir(parents=True, exist_ok=True)
    destination = inbox / (value["id"] + ".json")
    temporary = destination.with_suffix(".tmp")
    temporary.write_text(json.dumps(value))
    os.replace(temporary, destination)


def handle_prompt(text, state):
    position = text.find('{"project":')
    if position < 0:
        state["clarification"] = text
        return "Clarification recorded. Use Draft proposal when ready."
    bundle, _ = json.JSONDecoder().raw_decode(text[position:])
    inbox_match = re.search(r"atomically rename it into (.*?) with a unique", text)
    inbox = Path(inbox_match[1])
    request = bundle["request"]
    assert Path(state["cwd"]).resolve() == Path(bundle["project"]["root"]).resolve()
    if request["kind"] == "exploration":
        state["intent"] = request["intent"]
        return "What would success look like for this project?"
    if request["kind"] == "proposal":
        operation(bundle, inbox, "draft", request=request["id"],
                  body="# Proposal\n" + state.get("intent", "Existing work")
                  + "\n" + state.get("clarification", "Success remains an open question."))
    elif request["kind"] == "implementation":
        root = Path(bundle["project"]["root"])
        (root / "implementation.txt").write_text("Before\n")
        operation(bundle, inbox, "progress", body="Implementation ready for review.")
    elif request["kind"] == "explanation":
        operation(bundle, inbox, "explanation", request=request["id"],
                  artefact=request["artefact"], version=request["version"],
                  summary="Implement the agreed proposal with a reviewable change.",
                  walkthrough=[{"id": "behaviour", "title": "Agreed behaviour",
                                "body": "A small change fulfils the proposal.",
                                "basis": "recorded", "files": ["implementation.txt"],
                                "validation": "Fixture file contents checked."}])
    elif request["kind"] == "feedback":
        for artefact in bundle["artefacts"]:
            if artefact["kind"] == "markdown":
                source = Path(artefact["path"])
            elif artefact["kind"] == "code":
                source = Path(artefact["path"]) / "implementation.txt"
            else:
                continue
            if source.exists():
                source.write_text(source.read_text().replace("Before", "After"))
        for item in bundle["annotations"]:
            annotation = item["annotation"]
            identifier = annotation["id"] if isinstance(annotation, dict) else annotation
            operation(bundle, inbox, "reply", annotation=identifier,
                      body="Revised the artefact and checked its contents.")
    elif request["kind"] == "outcome":
        operation(bundle, inbox, "draft", request=request["id"],
                  body="Outcome from the real ACP fixture.")
    return "Fixture turn complete."


def chunk(session_id, text):
    emit({"method": "session/update", "params": {
        "sessionId": session_id,
        "update": {"sessionUpdate": "agent_message_chunk",
                   "content": {"type": "text", "text": text}},
    }})


PLAN = "# Plan\nRead the drive library, then report on its watchers."


def reach_egress_server():
    """Hold a connection to PA_EGRESS_PORT on loopback, if set, long enough
    for socket samples to see it, sending PA_EGRESS_CANARY over it."""
    port = os.environ.get("PA_EGRESS_PORT")
    if not port:
        return
    import socket
    with socket.create_connection(("127.0.0.1", int(port)), timeout=5) as s:
        s.sendall(b"GET /" + os.environ.get("PA_EGRESS_CANARY", "").encode()
                  + b" HTTP/1.1\r\nHost: example.com\r\n\r\n")
        time.sleep(float(os.environ.get("PA_EGRESS_HOLD", "2.5")))


def sandboxed_delegate(role, text, state):
    """Try to commit, write a ref and git config, then propose a commit.

    A sandboxed delegate's git metadata is read-only, so each attempt
    should be refused.  NO-COMMIT-BLOCK leaves the block out."""
    cwd = state["cwd"]
    run = lambda *args: subprocess.run(["git", *args], cwd=cwd,
                                       capture_output=True, text=True)
    Path(cwd, role + ".txt").write_text(role + "\n")
    attempts = {}
    added = run("add", role + ".txt").returncode == 0
    committed = run("-c", "user.name=Sandbox", "-c", "user.email=s@example.com",
                    "commit", "-q", "-m", "sneaky").returncode == 0
    attempts["commit"] = "ok" if added and committed else "refused"
    common = Path(cwd, run("rev-parse", "--git-common-dir").stdout.strip())

    def attempt(name, path, text):
        try:
            Path(path).parent.mkdir(parents=True, exist_ok=True)
            with open(path, "a") as stream:
                stream.write(text)
            attempts[name] = "ok"
        except OSError:
            attempts[name] = "refused"

    attempt("ref", common / "refs" / "heads" / "sneaky", "0" * 40 + "\n")
    attempt("config", common / "config", "[user]\n\tname = Sneaky\n")
    attempt("pointer", Path(cwd, ".git"), "\n")
    report = (f"DELEGATE role={role} mode={state['mode']} "
              + " ".join(f"{key}={value}" for key, value in attempts.items()))
    if "NO-COMMIT-BLOCK" in text:
        return report
    block = json.dumps({"version": 1, "subject": f"[{role}] Add {role}.txt",
                        "body": "Write the file.\n\nChecked by the fixture."})
    return report + "\n\n```pstack-commit\n" + block + "\n```\n"


def pstack_delegate(role, text, state):
    """Commit a file as a delegate, optionally start a writer, and report.

    WRITER LOG PIDFILE leaves an orphaned loop appending to LOG, with its
    pid in PIDFILE; LEAVE-DIRTY leaves an uncommitted file behind."""
    if "Leave your changes uncommitted" in text:
        return sandboxed_delegate(role, text, state)
    cwd = state["cwd"]
    git = lambda *args: subprocess.run(["git", *args], cwd=cwd, check=True,
                                       capture_output=True, text=True)
    Path(cwd, role + ".txt").write_text(role + "\n")
    git("add", role + ".txt")
    git("commit", "-q", "-m", f"Add {role}.txt")
    writer = re.search(r"WRITER (\S+) (\S+)", text)
    if writer:
        subprocess.Popen(
            ["/bin/sh", "-c",
             '(while :; do date >> "$1"; sleep 0.1; done) & echo $! > "$2"',
             "sh", writer[1], writer[2]],
            cwd="/", stdin=subprocess.DEVNULL, stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL).wait()
    if "LEAVE-DIRTY" in text:
        Path(cwd, "loose.txt").write_text("loose\n")
    branch = git("branch", "--show-current").stdout.strip()
    return f"DELEGATE role={role} mode={state['mode']} branch={branch}"


CAPACITY = "Selected model is at capacity. Please try a different model."


def provider_failure(message, text, state):
    """Fail the turn as a provider would, or return None.

    PROVIDER-FAIL STYLE TIMES in a brief fails it and the next TIMES-1
    prompts that ask to continue; a later one reports.  STYLE `codex'
    sends Codex's retry warning, then its message as the final text;
    `capacity' writes only the capacity line; `error:KIND' fails the
    prompt with a JSON-RPC error whose data has errorKind KIND."""
    marker = re.search(r"PROVIDER-FAIL (\S+) (\d+)", text)
    if "PSTACK-LEAD " in text:
        return None
    if marker:
        state["failure"] = marker[1]
        state["failures"] = int(marker[2])
    elif "Continue where you left off" not in text:
        return None
    if not state.get("failures"):
        return f"REPORT continued after {state.pop('failure', 'nothing')}"
    state["failures"] -= 1
    session_id = message["params"]["sessionId"]
    style = state["failure"]
    if style == "codex":
        emit({"method": "session/update", "params": {
            "sessionId": session_id,
            "update": {"sessionUpdate": "session_info_update",
                       "_meta": {"codex": {"error": {
                           "message": CAPACITY,
                           "codexErrorInfo": "serverOverloaded",
                           "turnId": "turn-1", "willRetry": True}}}}}})
        chunk(session_id, CAPACITY + "\n\n")
    elif style == "capacity":
        chunk(session_id, CAPACITY)
    else:
        chunk(session_id, "Partial before the failure.")
        emit({"id": message["id"],
              "error": {"code": -32603, "message": "Internal error",
                        "data": {"errorKind": style.split(":", 1)[1]}}})
        return False
    emit({"id": message["id"], "result": {"stopReason": "end_turn"}})
    return False


def handle_pstack_prompt(message, text, state):
    """Answer pstack panel traffic; return the reply, or None if unrelated.

    A reply of False means the turn stays open."""
    session_id = message["params"]["sessionId"]
    # A follow-up can quote member briefs, so recognise it first.
    if "has finished:" in text and "pstack panel" in text:
        return f"TRIAGED {text.count(chr(10) + '## ')}"
    delegate = re.search(r"You are the (\S+) delegate of a pstack panel", text)
    if delegate:
        if "NARRATE" in text:
            chunk(session_id, "I will add the file.")
            emit({"method": "session/update", "params": {
                "sessionId": session_id,
                "update": {"sessionUpdate": "tool_call", "toolCallId": "write-1",
                           "title": "Write builder file", "kind": "edit",
                           "status": "completed"},
            }})
        reply = pstack_delegate(delegate[1], text, state)
        failure = provider_failure(message, text, state)
        if failure is not None:
            return failure
        if "HANG" in text:
            chunk(session_id, f"PARTIAL {reply}")
            return False
        return reply
    candidate = re.search(r"You are the (\S+) candidate of a pstack arena", text)
    if candidate:
        role = candidate[1]
        Path(state["cwd"], role + ".txt").write_text(role + "\n")
        reach_egress_server()
        if "HANG" in text:
            chunk(session_id, f"PARTIAL role={role}")
            return False
        rubric = "seen" if "RUBRIC-" in text else "unseen"
        lead = "none"
        if os.environ.get("PA_LEAD"):
            try:
                Path(os.environ["PA_LEAD"]).read_text()
                lead = "readable"
            except OSError:
                lead = "denied"
        status = subprocess.run(["git", "status", "--porcelain"],
                                cwd=state["cwd"], capture_output=True,
                                text=True)
        git = ("ok" if status.returncode == 0
               and f"?? {role}.txt" in status.stdout else "failed")
        return (f"RATIONALE role={role} mode={state['mode']} rubric={rubric}"
                f" lead={lead} git={git}."
                " Considered editing in place; rejected it for a new file.")
    if "judge of a pstack arena" in text:
        reach_egress_server()
        lead = "none"
        if os.environ.get("PA_LEAD"):
            try:
                Path(os.environ["PA_LEAD"]).read_text()
                lead = "readable"
            except OSError:
                lead = "denied"
        checkout = re.search(r"Checkout: (\S+)", text)
        try:
            Path(checkout[1], "opus.txt").read_text()
            read = "ok"
        except (OSError, TypeError):
            read = "denied"
        try:
            Path(state["cwd"], "judge.txt").write_text("judge\n")
            write = "allowed"
        except OSError:
            write = "denied"
        return (f"SCORES mode={state['mode']} lead={lead} checkout={read}"
                f" write={write}\n" + text)
    failure = provider_failure(message, text, state)
    if failure is not None:
        return failure
    if "member of a pstack" in text:
        if "MENTION-CAPACITY" in text:
            return ("REPORT The drive library has spare capacity: each "
                    "watcher is cheap, and the panel never runs more than "
                    "four members, so " + CAPACITY.lower()
                    + " is a message it never needs to show.")
        if "HANG" in text:
            chunk(session_id, "Partial before hang.")
            return False
        if "ASK-PERMISSION" in text:
            request_id = "permission-" + uuid.uuid4().hex
            pending[request_id] = (session_id, message["id"])
            emit({"id": request_id, "method": "session/request_permission",
                  "params": {"sessionId": session_id,
                             "toolCall": {"toolCallId": "exit-plan",
                                          "title": "Ready to code?",
                                          "kind": "switch_mode",
                                          "status": "pending",
                                          "rawInput": {"plan": PLAN}},
                             "options": [
                                 {"optionId": "allow", "kind": "allow_once",
                                  "name": "Allow"},
                                 {"optionId": "reject", "kind": "reject_once",
                                  "name": "Reject"}]}})
            return False
        chunk(session_id, "Looking around.")
        emit({"method": "session/update", "params": {
            "sessionId": session_id,
            "update": {"sessionUpdate": "tool_call", "toolCallId": "read-1",
                       "title": "Read file", "kind": "read",
                       "status": "completed"},
        }})
        return f"REPORT cwd={state['cwd']} mode={state['mode']}"
    position = text.find("PSTACK-LEAD ")
    if position >= 0:
        request, _ = json.JSONDecoder().raw_decode(
            text[position + len("PSTACK-LEAD "):])
        block = ("Requesting a panel.\n\n```pstack-panel\n"
                 + json.dumps(request) + "\n```\n")
        if "KEEP-WORKING" not in text:
            return block
        chunk(session_id, block + "\nMeanwhile, more digging.")
        emit({"method": "session/update", "params": {
            "sessionId": session_id,
            "update": {"sessionUpdate": "tool_call", "toolCallId": "run-1",
                       "title": "Run git show", "kind": "execute",
                       "status": "in_progress"},
        }})
        working[session_id] = message["id"]
        return False
    return None


BENCH_MARKDOWN = """## Findings for section {n}

The drive watcher **parses every chunk** and keeps `:turn` and `:final`
in step, so a long turn copies its text again on each `concat`.  See
[the drive library](lisp/+agent-shell-drive.el) for the watcher table.

- Shells keep one upstream subscription each.
- Watchers are keyed by `(OWNER . ID)` and receive parsed plists.
- A read-only shell answers its own permission requests.

```elisp
(defun +bench-sample-{n} (shell state event)
  "Update SHELL's turn STATE from EVENT."
  (let ((kind (map-elt event :event)))
    (pcase kind
      ('agent-message-chunk
       (plist-put state :turn (concat (plist-get state :turn) "x")))
      (_ nil))))
```

| Scenario | Shells | Buffer | Chunks | Median ms |
|----------|--------|--------|--------|-----------|
| fresh    | 1      | 0 MB   | 600    | 0.{n}     |
| preload  | 1      | 2.4 MB | 600    | 1.{n}     |
| panel    | 3      | 0 MB   | 1800   | 2.{n}     |
| mixed    | 3      | 2.4 MB | 1800   | 3.{n}     |

1. Measure before changing anything.
2. Profile the worst realistic scenario in a run you do not report.
3. Map each hot spot to `file:line` and *name* the limiter.

```python
def render(rows):
    for row in rows:
        yield "| " + " | ".join(str(cell) for cell in row) + " |"
```

"""


def bench_text(n):
    parts = []
    index = 0
    while sum(map(len, parts)) < n:
        parts.append(BENCH_MARKDOWN.format(n=index))
        index += 1
    return "".join(parts)


def bench_update(session_id, update):
    emit({"method": "session/update",
          "params": {"sessionId": session_id, "update": update}})


def bench_stream(session_id, spec):
    """Stream a turn shaped by SPEC; return the final chunk text.

    SPEC keys: chunks, delay_ms, tools, tool_bytes, thoughts, seed, tag."""
    rng = random.Random(spec.get("seed", 1))
    chunks = spec.get("chunks", 600)
    tools = spec.get("tools", 0)
    tool_bytes = spec.get("tool_bytes", 2000)
    delay = spec.get("delay_ms", 0) / 1000
    tag = spec.get("tag", "0")
    text = bench_text(chunks * 80)
    every = chunks // (tools + 1) if tools else chunks + 1
    for _ in range(spec.get("thoughts", 0)):
        bench_update(session_id, {"sessionUpdate": "agent_thought_chunk",
                                  "content": {"type": "text",
                                              "text": "Thinking it over. "}})
    position = 0
    tool = 0
    for index in range(chunks):
        if index and index % every == 0 and tool < tools:
            call_id = f"bench-{tag}-{tool}"
            command = f"cd /repo && rg -n 'pattern{tool}' lisp/"
            bench_update(session_id, {
                "sessionUpdate": "tool_call", "toolCallId": call_id,
                "title": command, "kind": "execute", "status": "pending",
                "rawInput": {"command": command,
                             "description": "Search the repo"}})
            output = "".join(
                f"lisp/+agent-shell-drive.el:{line}: (pattern{tool} {line})\n"
                for line in range(tool_bytes // 48 + 1))[:tool_bytes]
            bench_update(session_id, {
                "sessionUpdate": "tool_call_update", "toolCallId": call_id,
                "status": "completed",
                "content": [{"type": "content",
                             "content": {"type": "text", "text": output}}]})
            tool += 1
        size = rng.randint(8, 72)
        piece = text[position:position + size]
        position += size
        bench_update(session_id, {"sessionUpdate": "agent_message_chunk",
                                  "content": {"type": "text", "text": piece}})
        if delay:
            time.sleep(delay)
    return f"\n\nBENCH-END-{tag}\n"



def sandbox_probe(text, state):
    """Write inside and outside the cwd, read a secret, start late writers.

    One late writer is a child working outside the cwd; the other is an
    orphan working inside it.  Both touch a file in the cwd after a delay."""
    marker = "SANDBOX-PROBE "
    spec, _ = json.JSONDecoder().raw_decode(text[text.find(marker) + len(marker):])
    cwd = Path(state["cwd"])
    results = {"mode": state["mode"], "env": os.environ.get("PA_ENV"),
               "tmpdir": os.environ.get("TMPDIR")}

    def attempt(name, action):
        try:
            action()
            results[name] = "ok"
        except OSError as error:
            results[name] = "denied " + str(error.errno)

    attempt("inside", lambda: (cwd / "inside.txt").write_text("inside\n"))
    attempt("outside", lambda: Path(spec["outside"]).write_text("outside\n"))
    attempt("secret", lambda: Path(spec["secret"]).read_text())
    quiet = {"stdin": subprocess.DEVNULL, "stdout": subprocess.DEVNULL,
             "stderr": subprocess.DEVNULL, "start_new_session": True}
    delay = str(spec["delay"])
    subprocess.Popen(["/bin/sh", "-c", 'sleep "$1"; touch "$2"', "sh", delay,
                      str(cwd / "late-child.txt")], cwd="/", **quiet)
    subprocess.Popen(["/bin/sh", "-c", '(sleep "$1"; touch late-orphan.txt) &',
                      "sh", delay], cwd=cwd, **quiet)
    return "PROBE " + json.dumps(results)


REFUSE_SET_MODE = "--refuse-set-mode" in sys.argv
MODES = [{"id": mode, "name": mode} for mode in ("default", "plan", "read-only",
                                                 "agent-full-access")]

sessions = {}
pending = {}
working = {}
for line in sys.stdin:
    message = json.loads(line)
    method = message.get("method")
    params = message.get("params", {})
    result = {}
    if method is None:
        session_id, prompt_id = pending.pop(message["id"])
        option = message.get("result", {}).get("outcome", {}).get("optionId")
        chunk(session_id, f"PERMISSION {option}")
        emit({"id": prompt_id, "result": {"stopReason": "end_turn"}})
        continue
    if method == "initialize":
        result = {
            "protocolVersion": params["protocolVersion"],
            "agentCapabilities": {},
            "authMethods": [],
            "agentInfo": {"name": "project-fixture", "version": "1"},
        }
    elif method == "session/new":
        result = {"sessionId": uuid.uuid4().hex,
                  "modes": {"currentModeId": "default",
                            "availableModes": MODES}}
        sessions[result["sessionId"]] = {"cwd": params["cwd"],
                                         "mode": "default"}
    elif method == "session/set_mode":
        if REFUSE_SET_MODE:
            emit({"id": message["id"],
                  "error": {"code": -32602, "message": "Mode refused"}})
            continue
        sessions[params["sessionId"]]["mode"] = params["modeId"]
    elif method == "session/prompt":
        text = "\n".join(item.get("text", "") for item in params["prompt"])
        state = sessions[params["sessionId"]]
        bench = text.find("BENCH-STREAM ")
        if bench >= 0:
            spec, _ = json.JSONDecoder().raw_decode(
                text[bench + len("BENCH-STREAM "):])
            chunk(params["sessionId"], bench_stream(params["sessionId"], spec))
            emit({"id": message["id"], "result": {"stopReason": "end_turn"}})
            continue
        if "SANDBOX-PROBE " in text:
            reply = sandbox_probe(text, state)
        else:
            reply = handle_pstack_prompt(message, text, state)
        if reply is False:
            continue
        if reply is None:
            reply = handle_prompt(text, state)
        chunk(params["sessionId"], reply)
        result = {"stopReason": "end_turn"}
    elif method == "session/cancel":
        prompt_id = working.pop(params["sessionId"], None)
        if prompt_id is not None:
            chunk(params["sessionId"], "CANCELLED")
            emit({"id": prompt_id, "result": {"stopReason": "cancelled"}})
        continue
    if "id" in message:
        emit({"id": message["id"], "result": result})
