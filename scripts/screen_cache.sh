#!/usr/bin/env bash
#############################################################
## scripts/screen_cache.sh                                 ##
## Checkpoint / restore a sensitivity screen's point cache ##
#############################################################
#
# A production screen is hours of compute in one long-lived process.
# run_morris() and run_sobol() take a cache_dir so an interrupted screen
# resumes rather than restarting, but that cache is an ordinary working file:
# on a host that reclaims its filesystem the cache dies with the run, and
# resumability buys nothing. This script gives the cache somewhere durable by
# parking it on its own git ref.
#
# Usage:
#   scripts/screen_cache.sh save    <cache-file> <ref-name>
#   scripts/screen_cache.sh restore <cache-file> <ref-name>
#
# Example, checkpointing an r=20 Morris screen every few minutes:
#   scripts/screen_cache.sh save outputs/cache/morris/points.csv morris-r20
#   scripts/screen_cache.sh restore outputs/cache/morris/points.csv morris-r20
#
# The ref is written with plumbing (hash-object / mktree / commit-tree), so
# neither the working tree, the index, nor the current branch is touched: a
# checkpoint cannot disturb the branch a run is being developed on. Each save
# replaces the previous commit rather than stacking, so the ref stays one
# commit deep and never merges.
#
# The ref name must identify the design, not just the screen. A screen's
# design follows from its seed, its parameter set and bounds, and its size
# (Morris r, Sobol n_sobol); a cache from one design is meaningless against
# another, which is why the examples above carry r20 in the name. Restoring
# across designs is the one way to corrupt a screen with this script, and
# naming the ref for the design is what prevents it.
#
# restore only overwrites when the ref holds strictly more rows than the local
# file, so it is safe to call before every launch attempt: a run further ahead
# than its last checkpoint is never rewound.
set -uo pipefail

REF_PREFIX="refs/heads/wip/screen-cache"
action="${1:-}"; cache_file="${2:-}"; ref_name="${3:-}"

if [ -z "$action" ] || [ -z "$cache_file" ] || [ -z "$ref_name" ]; then
  awk '/^# Usage:/{f=1} f&&/^#/{print substr($0,3)} f&&!/^#/{exit}' "$0" >&2
  exit 2
fi
ref="${REF_PREFIX}-${ref_name}"

count() { [ -s "$1" ] && grep -c . "$1" 2>/dev/null || echo 0; }

push_with_retry() {
  for i in 1 2 3 4; do
    git push -q -f origin "$1" 2>/dev/null && return 0
    sleep $((2 ** i))
  done
  return 1
}

case "$action" in
  save)
    [ -s "$cache_file" ] || { echo "screen_cache: nothing to save"; exit 0; }
    n=$(count "$cache_file")
    blob=$(git hash-object -w "$cache_file") || exit 1
    tree=$(printf '100644 blob %s\tpoints.csv\n' "$blob" | git mktree) || exit 1
    commit=$(git commit-tree "$tree" -m "screen cache ${ref_name}: ${n} rows") || exit 1
    git update-ref "$ref" "$commit" || exit 1
    push_with_retry "$ref" && echo "screen_cache: saved ${n} rows to ${ref_name}" \
      || echo "screen_cache: saved ${n} rows locally, push failed"
    ;;
  restore)
    for i in 1 2 3 4; do
      git fetch -q origin "${ref}:refs/remotes/origin/screen-cache-${ref_name}" --force 2>/dev/null && break
      sleep $((2 ** i))
    done
    tmp=$(mktemp)
    git show "origin/screen-cache-${ref_name}:points.csv" > "$tmp" 2>/dev/null
    remote=$(count "$tmp"); local_rows=$(count "$cache_file")
    if [ "$remote" -gt "$local_rows" ]; then
      mkdir -p "$(dirname "$cache_file")"
      cp "$tmp" "$cache_file"
      echo "screen_cache: restored ${remote} rows from ${ref_name} (local had ${local_rows})"
    else
      echo "screen_cache: local cache current (${local_rows} rows, ref has ${remote})"
    fi
    rm -f "$tmp"
    ;;
  *)
    echo "screen_cache: unknown action '${action}'" >&2; exit 2 ;;
esac
