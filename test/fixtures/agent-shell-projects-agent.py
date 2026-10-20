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


for line in sys.stdin:
    message = json.loads(line)
    method = message.get("method")
    params = message.get("params", {})
    result = {}
    if method == "initialize":
        result = {
            "protocolVersion": params["protocolVersion"],
            "agentCapabilities": {},
            "authMethods": [],
            "agentInfo": {"name": "project-fixture", "version": "1"},
        }
    elif method == "session/new":
        result = {"sessionId": uuid.uuid4().hex}
    elif method == "session/prompt":
        text = "\n".join(item.get("text", "") for item in params["prompt"])
        position = text.find('{"project":')
        bundle, _ = json.JSONDecoder().raw_decode(text[position:])
        inbox_match = re.search(r"atomically rename it into (.*?) with a unique", text)
        inbox = Path(inbox_match[1])
        request = bundle["request"]
        if request["kind"] == "feedback":
            for artefact in bundle["artefacts"]:
                if artefact["kind"] == "markdown":
                    source = Path(artefact["path"])
                    source.write_text(source.read_text().replace("Before", "After"))
            for item in bundle["annotations"]:
                annotation = item["annotation"]
                identifier = annotation["id"] if isinstance(annotation, dict) else annotation
                operation(bundle, inbox, "reply", annotation=identifier,
                          body="Revised the document and checked its contents.")
        elif request["kind"] == "outcome":
            operation(bundle, inbox, "draft", request=request["id"],
                      body="Outcome from the real ACP fixture.")
        emit({"method": "session/update", "params": {
            "sessionId": params["sessionId"],
            "update": {"sessionUpdate": "agent_message_chunk",
                       "content": {"type": "text", "text": "Fixture turn complete."}},
        }})
        result = {"stopReason": "end_turn"}
    elif method == "session/cancel":
        continue
    if "id" in message:
        emit({"id": message["id"], "result": result})
