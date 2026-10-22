"""Deterministic ACP peer for exercising the real Emacs agent-shell bridge."""

import json
import os
from pathlib import Path
import re
import sys
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


def handle_pstack_prompt(message, text, state):
    """Answer pstack panel traffic; return the reply, or None if unrelated.

    A reply of False means the turn stays open."""
    session_id = message["params"]["sessionId"]
    if "member of a pstack" in text:
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
    if "has finished:" in text and "pstack panel" in text:
        return f"TRIAGED {text.count(chr(10) + '## ')}"
    position = text.find("PSTACK-LEAD ")
    if position >= 0:
        request, _ = json.JSONDecoder().raw_decode(
            text[position + len("PSTACK-LEAD "):])
        return ("Requesting a panel.\n\n```pstack-panel\n"
                + json.dumps(request) + "\n```\n")
    return None


REFUSE_SET_MODE = "--refuse-set-mode" in sys.argv
MODES = [{"id": mode, "name": mode} for mode in ("default", "plan", "read-only")]

sessions = {}
pending = {}
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
        reply = handle_pstack_prompt(message, text, state)
        if reply is False:
            continue
        if reply is None:
            reply = handle_prompt(text, state)
        chunk(params["sessionId"], reply)
        result = {"stopReason": "end_turn"}
    elif method == "session/cancel":
        continue
    if "id" in message:
        emit({"id": message["id"], "result": result})
