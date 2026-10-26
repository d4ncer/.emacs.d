#!/bin/bash
# Usage: +agent-shell-sandbox-launch.sh NAMES BWRAP-ARGUMENT...
#
# Run bwrap with BWRAP-ARGUMENTs in a new user and network namespace whose
# only route out is slirp4netns, attached before bwrap starts.  bwrap gets
# an empty environment holding only the variables NAMES lists, separated
# by spaces, with their values here.  Those reach it through a pipe, so no
# value appears in a command line.
#
# Killing this script ends everything: bwrap has --die-with-parent, its
# PID namespace takes the sandbox's processes with it, and slirp4netns
# exits once the pipe this script holds closes.  On SIGTERM it kills the
# namespace's first process and exits only once every process in that
# namespace has.

set -euo pipefail

names=$1
shift

dir=$(mktemp -d)
mkfifo "$dir/go" "$dir/ready" "$dir/exit" "$dir/info"
# Opened read-write so no open blocks, then unlinked: the pipes need no
# paths, and nothing else can open them.
exec 5<>"$dir/go" 6<>"$dir/ready" 7<>"$dir/exit" 8<"$dir/exit" \
     9<>"$dir/info"
rm -rf "$dir"

environment() {
  printf '%s\0' --clearenv
  local name
  for name in $names; do
    printf '%s\0' --setenv "$name" "${!name}"
  done
}

# The namespace's first process waits for "go" before it becomes bwrap.
unshare --user --map-root-user --net -- /bin/bash -c '
  read -r -t 30 -u 5 go && [ "$go" = go ] || exit 125
  exec "$@" 5<&-' pa-sandbox \
  bwrap --args 3 --info-fd 4 "$@" \
  0<&0 3< <(environment) 4>&9 6>&- 7>&- 8<&- 9>&- &
inner=$!

# bwrap writes its info, naming the namespace's first process, before it
# runs the command.  Killing that process kills the whole namespace, and
# bwrap exits only once it has.
child=
terminate() {
  local line
  while [ -z "$child" ] && read -r -t 1 -u 9 line; do
    [[ $line =~ \"child-pid\":\ *([0-9]+) ]] && child=${BASH_REMATCH[1]}
  done
  kill -KILL "${child:-$inner}" 2>/dev/null || true
}
trap terminate TERM INT HUP

# unshare has made the namespaces once the process runs bash.
for _ in $(seq 500); do
  [ "$(cat "/proc/$inner/comm" 2>/dev/null)" = bash ] && break
  kill -0 "$inner" 2>/dev/null || { wait "$inner"; exit $?; }
  sleep 0.01
done
if [ "$(readlink "/proc/$inner/ns/net")" = "$(readlink /proc/self/ns/net)" ]
then
  echo "pa-sandbox: no network namespace for $inner" >&2
  kill -KILL "$inner"
  exit 1
fi

slirp4netns --configure --mtu=65520 --disable-host-loopback \
  --ready-fd=6 --exit-fd=8 "$inner" tap0 \
  </dev/null >/dev/null 2>&1 5<&- 7>&- 9<&- &
slirp=$!

if ! read -r -t 15 -n 1 -u 6 ready || [ "$ready" != 1 ]; then
  echo "pa-sandbox: slirp4netns did not attach" >&2
  kill -KILL "$inner" "$slirp" 2>/dev/null || true
  exit 1
fi
exec 6>&- 8<&-
echo go >&5

status=0
while true; do
  wait "$inner" && status=0 || status=$?
  kill -0 "$inner" 2>/dev/null || break
done
kill "$slirp" 2>/dev/null || true
wait "$slirp" || true
exit "$status"
