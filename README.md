# Advent of Code in Roc

Advent of Code solutions grouped by year and day.

## Requirements

- A current Roc compiler on `PATH`.
- Internet access the first time Roc downloads the pinned `basic-cli` platform release.

Solutions read puzzle input from standard input:

```sh
roc 2024/01.roc < input.txt
```

Run formatting, checking, and tests for every solution. The script prints each stage as it runs, followed by a summary and any failure details:

```sh
./ci/all_tests.sh
```

Set `ROC` to test with a different compiler binary:

```sh
ROC=/path/to/roc ./ci/all_tests.sh
```

The runner allows up to 200 MB of transitive Roc packages for the pinned platform. Override this with `ROC_MAX_TRANSITIVE_MB` if needed.

GitHub Actions runs the same checks against the latest nightly Roc compiler.
