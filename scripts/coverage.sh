#!/usr/bin/env bash
set -euo pipefail

# Run tests with coverage instrumentation and check threshold.
# Requires the 'undercover' Emacs package.

cd "$(git rev-parse --show-toplevel)"

COVERAGE_MIN=${COVERAGE_MIN:-60}

mkdir -p coverage

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

echo "Running tests with coverage instrumentation..."
emacs -batch -L . \
  --eval "(require 'undercover)" \
  --eval "(undercover \"ecard.el\" \"ecard-compat.el\" \"ecard-carddav.el\"
                      \"ecard-carddav-map.el\" \"ecard-carddav-auth.el\"
                      \"ecard-carddav-sync.el\" \"ecard-org.el\"
                      \"ecard-display.el\" \"ecard-sync.el\"
                      \"ecard-tools.el\" \"ecard-widget.el\"
                      (:report-file \"coverage/lcov.info\")
                      (:report-format 'lcov)
                      (:send-report nil))" \
  "${LOAD_ARGS[@]}" \
  -f ert-run-tests-batch-and-exit

if [ ! -f coverage/lcov.info ]; then
  echo "WARNING: No coverage report generated"
  exit 0
fi

# Parse lcov.info for coverage percentage
total_lines=$(grep -c "^DA:" coverage/lcov.info || echo 0)
covered_lines=$(grep "^DA:" coverage/lcov.info | grep -cv ",0$" || echo 0)

if [ "$total_lines" -gt 0 ]; then
  coverage=$((covered_lines * 100 / total_lines))
  echo "Coverage: ${coverage}% (${covered_lines}/${total_lines} lines)"
  if [ "$coverage" -lt "$COVERAGE_MIN" ]; then
    echo "ERROR: Coverage ${coverage}% is below ${COVERAGE_MIN}% threshold"
    exit 1
  fi
  echo "Coverage meets ${COVERAGE_MIN}% minimum threshold."
else
  echo "WARNING: No coverage data found in lcov.info"
fi
