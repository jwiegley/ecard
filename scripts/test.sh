#!/usr/bin/env bash
set -euo pipefail

# Run all ERT tests.

cd "$(git rev-parse --show-toplevel)"

TEST_FILES=(
  ecard-test.el
  ecard-carddav-map-test.el
  ecard-carddav-test.el
  ecard-compat-test.el
  ecard-display-test.el
  ecard-org-test.el
  ecard-regression-test.el
  ecard-sync-test.el
  ecard-tools-test.el
  ecard-widget-test.el
)

LOAD_ARGS=()
for f in "${TEST_FILES[@]}"; do
  LOAD_ARGS+=(-l "$f")
done

echo "Running tests from ${#TEST_FILES[@]} test files..."
emacs -batch -L . "${LOAD_ARGS[@]}" -f ert-run-tests-batch-and-exit
