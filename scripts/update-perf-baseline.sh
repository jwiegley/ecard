#!/usr/bin/env bash
set -euo pipefail

# Generate or update the performance baseline file.
# Run this after intentional performance changes.

cd "$(git rev-parse --show-toplevel)"

ITERATIONS=5

echo "Running benchmarks ($ITERATIONS iterations for stable measurement)..."

TOTAL_PARSE=0
TOTAL_SERIALIZE=0

for i in $(seq 1 $ITERATIONS); do
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
                (princ (format \"%.1f %.1f\"
                               (* 1000 parse-time)
                               (* 1000 serialize-time)))))" 2>/dev/null)

  PARSE=$(echo "$RESULT" | awk '{print $1}')
  SERIALIZE=$(echo "$RESULT" | awk '{print $2}')
  TOTAL_PARSE=$(echo "$TOTAL_PARSE + $PARSE" | bc -l)
  TOTAL_SERIALIZE=$(echo "$TOTAL_SERIALIZE + $SERIALIZE" | bc -l)
  echo "  Iteration $i: parse=${PARSE}ms serialize=${SERIALIZE}ms"
done

AVG_PARSE=$(echo "$TOTAL_PARSE / $ITERATIONS" | bc -l | xargs printf "%.1f")
AVG_SERIALIZE=$(echo "$TOTAL_SERIALIZE / $ITERATIONS" | bc -l | xargs printf "%.1f")

cat > .perf-baseline <<EOF
# Performance baseline for ecard
# Generated: $(date -u +%Y-%m-%dT%H:%M:%SZ)
# Iterations: $ITERATIONS
# Metric: 100 vCard parse/serialize cycle
parse_ms=$AVG_PARSE
serialize_ms=$AVG_SERIALIZE
EOF

echo ""
echo "Baseline written to .perf-baseline:"
echo "  parse_ms=$AVG_PARSE"
echo "  serialize_ms=$AVG_SERIALIZE"
echo ""
echo "Commit .perf-baseline to track performance over time."
