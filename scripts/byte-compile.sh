#!/usr/bin/env bash
set -euo pipefail

# Byte-compile all source files with warnings treated as errors.

cd "$(git rev-parse --show-toplevel)"

SOURCE_FILES=(
  ecard.el
  ecard-carddav-auth.el
  ecard-carddav-map.el
  ecard-carddav-mock.el
  ecard-carddav-sync.el
  ecard-carddav.el
  ecard-compat.el
  ecard-compat-examples.el
  ecard-display.el
  ecard-org.el
  ecard-sync.el
  ecard-tools-adapter.el
  ecard-tools.el
  ecard-widget.el
  ecard-benchmark.el
)

echo "Byte-compiling ${#SOURCE_FILES[@]} source files..."
emacs -batch -L . \
  --eval "(setq byte-compile-error-on-warn t)" \
  --eval "(setq byte-compile-docstring-max-column 1000)" \
  -f batch-byte-compile "${SOURCE_FILES[@]}"
echo "Byte-compilation succeeded with no warnings."
