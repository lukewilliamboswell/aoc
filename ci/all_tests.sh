#!/usr/bin/env bash

set -uo pipefail

ROC="${ROC:-roc}"
MAX_TRANSITIVE_MB="${ROC_MAX_TRANSITIVE_MB:-200}"
status=0
passed=0
failed=0

if [[ (-t 1 && -z "${NO_COLOR:-}") || -n "${FORCE_COLOR:-}" ]]; then
    reset=$'\033[0m'
    bold=$'\033[1m'
    red=$'\033[31m'
    green=$'\033[32m'
    cyan=$'\033[36m'
else
    reset=''
    bold=''
    red=''
    green=''
    cyan=''
fi

shopt -s nullglob
if (($# > 0)); then
    roc_files=("$@")
else
    roc_files=(20??/*.roc)
fi

if ((${#roc_files[@]} == 0)); then
    echo "No Roc solutions found matching 20??/*.roc" >&2
    exit 1
fi

run_check() {
    local file="$1"
    shift
    local stage="$1"
    shift

    local output
    output="$(mktemp)"

    printf '  %-5s ' "$stage"

    if ! "$ROC" "$@" "$file" >"$output" 2>&1; then
        printf '%sFAILED%s\n' "$red" "$reset"
        printf '%sFAILED:%s %s %s %s\n' "$red" "$reset" "$ROC" "$*" "$file" >&2
        sed 's/^/  /' "$output" >&2
        status=1
        ((failed += 1))
    else
        printf '%spassed%s\n' "$green" "$reset"
        ((passed += 1))
    fi

    rm -f "$output"
}

for file in "${roc_files[@]}"; do
    printf '%s%s%s\n' "$cyan" "$file" "$reset"
    run_check "$file" fmt fmt --check
    run_check "$file" check check --max-transitive-mb="$MAX_TRANSITIVE_MB"
    run_check "$file" test test --max-transitive-mb="$MAX_TRANSITIVE_MB"
done

echo
if ((failed == 0)); then
    summary_color="$green"
else
    summary_color="$red"
fi
printf '%s%sSummary:%s %d file(s), %d stage(s) passed, %d stage(s) failed\n' \
    "$bold" "$summary_color" "$reset" "${#roc_files[@]}" "$passed" "$failed"

exit "$status"
