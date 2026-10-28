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

# With PA_SANDBOX_EGRESS naming a private folder, tshark watches the
# namespace's tap0 from inside its user and network namespaces, before
# slirp4netns translates the packets, and writes chosen header fields to
# the folder's events.  It holds none of this script's descriptors and
# none of its environment, which carries the agent's login.  Every
# observer failure is written to the folder's status as a fixed code and
# returns normally, so the member starts and ends as it would without it.
egress=
egress_path=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
egress_filter='(tcp[tcpflags] & (tcp-syn|tcp-ack) == tcp-syn)
  or (tcp[((tcp[12:1] & 0xf0) >> 2):1] == 0x16
      and tcp[((tcp[12:1] & 0xf0) >> 2) + 5:1] == 0x01)
  or port 53 or (udp and src host 10.0.2.100)'
egress_fields='frame.time_epoch ip.dst ipv6.dst tcp.dstport udp.dstport
  tcp.flags.syn tcp.flags.ack dns.flags.response dns.qry.name dns.a dns.aaaa
  tls.handshake.extensions_server_name'

egress_start() {
  local dir=$PA_SANDBOX_EGRESS tshark nsenter field
  local -a fields=()
  tshark=$(PATH=$egress_path command -v tshark) &&
    nsenter=$(PATH=$egress_path command -v nsenter) ||
    { echo "unavailable helper-missing" >"$dir/status"; return 0; }
  mkdir -m 700 "$dir/tmp" ||
    { echo "unavailable observer-not-started" >"$dir/status"; return 0; }
  for field in $egress_fields; do fields+=(-e "$field"); done
  # Live capture spools packets to a temporary file in TMPDIR, so it
  # goes in the private folder, which Emacs deletes on close.  The size
  # limit bounds that file and the events; past it tshark gets SIGXFSZ.
  (
    ulimit -c 0
    ulimit -f 65536
    exec env -i PATH="$egress_path" LANG=C.UTF-8 TMPDIR="$dir/tmp" \
      "$nsenter" --target "$inner" --user --net --preserve-credentials -- \
      "$tshark" -l -n -i tap0 -s 4096 -f "$egress_filter" -a duration:21600 \
      -T fields -E separator=/t -E occurrence=a -E aggregator=, "${fields[@]}"
  ) </dev/null >"$dir/events" 2>"$dir/log" 5<&- 7>&- 9<&- &
  egress=$!
  echo "$egress $(sed 's/.*) //' "/proc/$egress/stat" 2>/dev/null |
    cut -d' ' -f20)" >"$dir/lease"
  for _ in $(seq 30); do
    if grep -q '^Capturing on' "$dir/log" 2>/dev/null; then
      echo ready >"$dir/status"
      return 0
    fi
    kill -0 "$egress" 2>/dev/null || break
    sleep 0.1
  done
  if kill -0 "$egress" 2>/dev/null; then echo "unavailable readiness-timeout"
  elif grep -q '^nsenter:' "$dir/log"; then
    echo "unavailable namespace-entry-failed"
  elif grep -qi 'permission' "$dir/log"; then
    echo "unavailable capture-permission-denied"
  else echo "unavailable observer-exited"
  fi >"$dir/status"
  return 0
}

# Stop the observer once the namespace's processes have exited, within a
# second, and record how it ended and what it reported dropping.
egress_stop() {
  local dir=$PA_SANDBOX_EGRESS early= code=0 dropped
  [ -n "$egress" ] || return 0
  if kill -0 "$egress" 2>/dev/null; then
    kill -TERM "$egress" 2>/dev/null
    for _ in $(seq 20); do
      kill -0 "$egress" 2>/dev/null || break
      sleep 0.05
    done
    kill -KILL "$egress" 2>/dev/null
  else
    early=" early"
  fi
  wait "$egress" || code=$?
  echo "exit $code$early" >>"$dir/status"
  dropped=$(grep -oE '[0-9]+ packets? dropped' "$dir/log" | head -n 1 |
    cut -d' ' -f1)
  [ -z "$dropped" ] || echo "dropped $dropped" >>"$dir/status"
  return 0
}

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
if [ -n "${PA_SANDBOX_EGRESS:-}" ]; then egress_start || true; fi
echo go >&5

status=0
while true; do
  wait "$inner" && status=0 || status=$?
  kill -0 "$inner" 2>/dev/null || break
done
egress_stop || true
kill "$slirp" 2>/dev/null || true
wait "$slirp" || true
exit "$status"
