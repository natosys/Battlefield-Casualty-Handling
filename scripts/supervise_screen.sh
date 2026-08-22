#!/usr/bin/env bash
#############################################################
## scripts/supervise_screen.sh                             ##
## Drive a long screen to completion across host failures  ##
#############################################################
#
# A production Morris or Sobol screen is hours of compute in one process. Two
# things can end it early: the process dies, or the host reclaims its
# filesystem and takes the point cache with it. run_morris()/run_sobol() handle
# resuming from a cache (see their cache_dir argument) and scripts/screen_cache.sh
# keeps that cache somewhere durable; this script is the loop that ties them
# together, so a screen restarts itself and never loses more than one
# checkpoint interval.
#
# Usage:
#   scripts/supervise_screen.sh <cache-file> <ref-name> <done-marker> <log> -- <command...>
#
# Example, an r=20 Morris screen that survives a host that keeps dying:
#   scripts/supervise_screen.sh outputs/cache/morris/points.csv morris-r20 \
#     "Sensitivity analysis complete" outputs/morris.log -- \
#     Rscript scripts/run_sensitivity.R --r 20 --cache-dir outputs/cache
#
# <done-marker> is a string the command prints when the screen is genuinely
# finished. The loop stops on that rather than on exit status, because a run
# killed with the host exits non-zero and a run that completed the design but
# failed while plotting exits non-zero too; only the marker distinguishes
# "finished" from "interrupted, resume it".
#
# Before each attempt the cache is restored from its ref, so a run that starts
# on a wiped filesystem picks up where the last checkpoint left off rather than
# at point one. While the command runs the cache is saved every
# CHECKPOINT_SECONDS (default 120), bounding what a sudden death can cost.
#
# The ref name must identify the design: see scripts/screen_cache.sh for why
# restoring a cache across designs is the one way to corrupt a screen.
set -uo pipefail

CHECKPOINT_SECONDS="${CHECKPOINT_SECONDS:-120}"
MAX_ATTEMPTS="${MAX_ATTEMPTS:-500}"

cache_file="${1:-}"; ref_name="${2:-}"; done_marker="${3:-}"; log="${4:-}"; sep="${5:-}"
if [ -z "$cache_file" ] || [ -z "$ref_name" ] || [ -z "$done_marker" ] || [ -z "$log" ] || [ "$sep" != "--" ]; then
  awk '/^# Usage:/{f=1} f&&/^#/{print substr($0,3)} f&&!/^#/{exit}' "$0" >&2
  exit 2
fi
shift 5
[ "$#" -gt 0 ] || { echo "supervise_screen: no command given" >&2; exit 2; }

here="$(cd "$(dirname "$0")" && pwd)"
rows() { [ -s "$cache_file" ] && grep -c . "$cache_file" 2>/dev/null || echo 0; }
finished() { [ -s "$log" ] && grep -q "$done_marker" "$log" 2>/dev/null; }

for attempt in $(seq 1 "$MAX_ATTEMPTS"); do
  finished && break
  "$here/screen_cache.sh" restore "$cache_file" "$ref_name" || true
  echo "### attempt ${attempt} $(date -u +%H:%M:%S) rows=$(rows)"
  "$@" > "$log" 2>&1 &
  pid=$!
  while kill -0 "$pid" 2>/dev/null; do
    sleep "$CHECKPOINT_SECONDS"
    "$here/screen_cache.sh" save "$cache_file" "$ref_name" >/dev/null 2>&1 || true
  done
  wait "$pid"; status=$?
  "$here/screen_cache.sh" save "$cache_file" "$ref_name" >/dev/null 2>&1 || true
  if finished; then
    echo "### finished after ${attempt} attempt(s), rows=$(rows)"
    break
  fi
  echo "### attempt ${attempt} ended (exit ${status}) without the done marker; resuming"
  sleep 5
done

"$here/screen_cache.sh" save "$cache_file" "$ref_name" >/dev/null 2>&1 || true
finished || { echo "### gave up after ${MAX_ATTEMPTS} attempts, rows=$(rows)" >&2; exit 1; }
