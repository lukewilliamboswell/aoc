#!/usr/bin/env bash

# https://vaneyckt.io/posts/safer_bash_scripts_with_set_euxo_pipefail/
set -euxo pipefail

if [ -z "${ROC:-}" ]; then
  echo "INFO: The ROC environment variable is not set."
  export ROC=$(which roc)
fi

for ROC_FILE in 2020/*.roc; do
    $ROC check $ROC_FILE
    $ROC test $ROC_FILE
done

for ROC_FILE in 2021/*.roc; do
    $ROC check $ROC_FILE
    $ROC test $ROC_FILE
done

for ROC_FILE in 2022/*.roc; do
    $ROC check $ROC_FILE
    $ROC test $ROC_FILE
done

for ROC_FILE in 2023/*.roc; do
    $ROC check $ROC_FILE
    $ROC test $ROC_FILE
done
