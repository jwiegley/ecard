#!/usr/bin/env bash
set -euo pipefail

# Run package-lint and checkdoc on the main entry point.

cd "$(git rev-parse --show-toplevel)"

echo "Running package-lint on ecard.el..."
emacs -batch -L . \
  --eval "(require 'package)" \
  --eval "(package-initialize)" \
  --eval "(require 'package-lint)" \
  -f package-lint-batch-and-exit ecard.el

echo "Running checkdoc on source files..."
SOURCE_FILES=(
  ecard.el
  ecard-carddav-auth.el
  ecard-carddav-map.el
  ecard-carddav-sync.el
  ecard-carddav.el
  ecard-compat.el
  ecard-display.el
  ecard-org.el
  ecard-sync.el
  ecard-tools.el
  ecard-widget.el
)

for f in "${SOURCE_FILES[@]}"; do
  emacs -batch -L . \
    --eval "(require 'checkdoc)" \
    --eval "(with-current-buffer (find-file-noselect \"$f\")
              (let ((warnings (checkdoc-current-buffer t)))
                (when warnings
                  (message \"checkdoc warnings in %s\" \"$f\"))))" || true
done

echo "Lint checks complete."
