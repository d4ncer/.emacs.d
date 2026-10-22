"""Filter a NUL-separated filename manifest; emit ranked JSON strings."""

import argparse
import heapq
import json
import os
import signal
import subprocess
import sys
import threading


class Cancelled(Exception):
    """An obsolete query was replaced or the picker was closed."""


class Job:
    def __init__(self):
        self.cancelled = threading.Event()
        self.lock = threading.Lock()
        self.processes = []

    def register(self, process):
        with self.lock:
            self.processes.append(process)
            if self.cancelled.is_set():
                self.terminate(process)

    @staticmethod
    def terminate(process):
        try:
            if process.poll() is None:
                process.terminate()
        except ProcessLookupError:
            pass

    def cancel(self):
        self.cancelled.set()
        with self.lock:
            for process in self.processes:
                self.terminate(process)

    def check(self):
        if self.cancelled.is_set():
            raise Cancelled()


def search(manifest, query, preferences, limit, rg, job=None):
    terms = list(dict.fromkeys(query.split()))
    recent = preferences.get("recent", [])
    included = preferences.get("include", [])
    excluded = set(preferences.get("exclude", []))
    proximity = preferences.get("directory", "")
    recency = {name: i for i, name in enumerate(recent)}
    processes = []
    stream = None
    own_job = job is None
    job = job or Job()

    def cancel(_signum, _frame):
        job.cancel()
        raise SystemExit(0)

    previous_handler = signal.signal(signal.SIGTERM, cancel) if own_job else None
    try:
        if terms and os.path.exists(manifest):
            for index, term in enumerate(terms):
                command = [rg, "--no-config", "--text", "--null-data",
                           "--ignore-case", "--fixed-strings", "--", term]
                if index == 0:
                    command.append(manifest)
                process = subprocess.Popen(
                    command, stdin=stream, stdout=subprocess.PIPE,
                    stderr=subprocess.PIPE)
                if stream is not None:
                    stream.close()
                processes.append(process)
                job.register(process)
                stream = process.stdout
            records = stream.read().split(b"\0")
            job.check()
            for process in processes:
                error = process.stderr.read()
                if process.wait() not in (0, 1):
                    job.check()
                    raise RuntimeError(error.decode("utf-8", "replace"))
            candidates = (record.decode("utf-8") for record in records if record)
        else:
            candidates = iter(())
        folded_terms = [term.lower() for term in terms]
        extra = (name for name in included
                 if all(term in name.lower() for term in folded_terms))
        seen = set()

        def eligible():
            for group in (candidates, extra):
                for name in group:
                    job.check()
                    if name not in excluded and name not in seen:
                        seen.add(name)
                        yield name

        def rank(name):
            folded = name.lower()
            basename = folded.rsplit("/", 1)[-1]
            stem = basename.rsplit(".", 1)[0]
            exact = any(term in (basename, stem) for term in folded_terms)
            basename_terms = sum(term in basename for term in folded_terms)
            return (not exact, -basename_terms,
                    recency.get(name, len(recent)),
                    not (proximity and name.startswith(proximity)),
                    len(name), folded, name)

        return heapq.nsmallest(limit, eligible(), key=rank)
    finally:
        if own_job:
            signal.signal(signal.SIGTERM, previous_handler)
        if stream is not None:
            stream.close()
        for process in processes:
            if process.poll() is None:
                process.terminate()
            process.wait()
            process.stderr.close()


def serve(rg):
    lock = threading.Lock()
    current = {"job": None}

    def run(request, job):
        try:
            candidates = search(request["manifest"], request["query"],
                                request["preferences"], request["limit"], rg, job)
            response = {"id": request["id"], "candidates": candidates}
        except Cancelled:
            return
        except Exception as error:
            response = {"id": request["id"], "error": str(error)}
        with lock:
            if current["job"] is job and not job.cancelled.is_set():
                print(json.dumps(response, ensure_ascii=True), flush=True)

    try:
        for line in sys.stdin:
            request = json.loads(line)
            with lock:
                if current["job"] is not None:
                    current["job"].cancel()
                current["job"] = None
                if request.get("query") is not None:
                    job = Job()
                    current["job"] = job
                    threading.Thread(target=run, args=(request, job),
                                     daemon=True).start()
    finally:
        if current["job"] is not None:
            current["job"].cancel()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--serve", action="store_true")
    parser.add_argument("--manifest")
    parser.add_argument("--query")
    parser.add_argument("--preferences")
    parser.add_argument("--limit", type=int, default=500)
    parser.add_argument("--rg", required=True)
    parser.add_argument("--generation", default="")
    args = parser.parse_args()
    if args.serve:
        serve(args.rg)
        return
    if any(value is None for value in
           (args.manifest, args.query, args.preferences)):
        parser.error("--manifest, --query and --preferences are required")
    if args.limit < 1:
        parser.error("--limit must be positive")
    with open(args.preferences, encoding="utf-8") as stream:
        preferences = json.load(stream)
    for name in search(args.manifest, args.query, preferences, args.limit, args.rg):
        print(json.dumps(name, ensure_ascii=True))


if __name__ == "__main__":
    main()
