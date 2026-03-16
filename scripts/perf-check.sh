#!/usr/bin/env bash
set -euo pipefail

# Run performance benchmarks and check for regressions against baseline.
# If .perf-baseline exists, fail if any metric regresses by more than 5%.
# If no baseline, report timings and pass.

cd "$(git rev-parse --show-toplevel)"

REGRESSION_THRESHOLD=5

echo "Running performance benchmarks..."
RESULT=$(emacs -batch -L . \
  -l ecard.el -l ecard-compat.el -l ecard-benchmark.el \
  --eval "(progn
            (let* ((num-vcards 100)
                   (content (ecard-benchmark--generate-test-file num-vcards))
                   (start (float-time))
                   (cards (ecard-compat-parse-multiple content))
                   (parse-time (- (float-time) start))
                   (start2 (float-time))
                   (serialized (mapconcat #'ecard-serialize cards \"\n\"))
                   (serialize-time (- (float-time) start2)))
              (princ (format \"parse_ms=%.1f serialize_ms=%.1f count=%d\"
                             (* 1000 parse-time)
                             (* 1000 serialize-time)
                             (length cards)))))" 2>/dev/null)

echo "Benchmark result: $RESULT"

# Extract values
PARSE_MS=$(echo "$RESULT" | sed 's/.*parse_ms=\([0-9.]*\).*/\1/')
SERIALIZE_MS=$(echo "$RESULT" | sed 's/.*serialize_ms=\([0-9.]*\).*/\1/')
COUNT=$(echo "$RESULT" | sed 's/.*count=\([0-9]*\).*/\1/')

echo "Parse 100 vCards: ${PARSE_MS}ms"
echo "Serialize 100 vCards: ${SERIALIZE_MS}ms"
echo "Cards processed: ${COUNT}"

if [ -f .perf-baseline ]; then
  echo ""
  echo "Checking against baseline..."
  BASELINE_PARSE=$(grep "^parse_ms=" .perf-baseline | cut -d= -f2)
  BASELINE_SERIALIZE=$(grep "^serialize_ms=" .perf-baseline | cut -d= -f2)

  check_regression() {
    local name="$1" current="$2" baseline="$3"
    local allowed
    allowed=$(echo "$baseline * (100 + $REGRESSION_THRESHOLD) / 100" | bc -l)
    if echo "$current > $allowed" | bc -l | grep -q 1; then
      echo "REGRESSION: $name: ${current}ms vs baseline ${baseline}ms (>${REGRESSION_THRESHOLD}% slower)"
      return 1
    else
      echo "OK: $name: ${current}ms (baseline: ${baseline}ms)"
      return 0
    fi
  }

  FAILED=0
  check_regression "parse" "$PARSE_MS" "$BASELINE_PARSE" || FAILED=1
  check_regression "serialize" "$SERIALIZE_MS" "$BASELINE_SERIALIZE" || FAILED=1

  if [ "$FAILED" -eq 1 ]; then
    echo ""
    echo "Performance regression detected. If this is expected, update baseline:"
    echo "  ./scripts/update-perf-baseline.sh"
    exit 1
  fi
else
  echo ""
  echo "No .perf-baseline found. Generate one with:"
  echo "  ./scripts/update-perf-baseline.sh"
fi

echo "Performance check passed."
