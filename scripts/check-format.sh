#!/usr/bin/env bash
set -euo pipefail

# Check that all Emacs Lisp files have correct indentation.
# Uses Emacs's built-in indent-region as the canonical standard.

cd "$(git rev-parse --show-toplevel)"

ALL_EL_FILES=($(ls *.el))
FAILED=0

for f in "${ALL_EL_FILES[@]}"; do
  result=$(emacs -batch -L . \
    --eval "(progn
              (find-file \"$f\")
              (emacs-lisp-mode)
              (setq-local indent-tabs-mode nil)
              (untabify (point-min) (point-max))
              (let ((original (buffer-string)))
                (indent-region (point-min) (point-max))
                (if (string= original (buffer-string))
                    (princ \"OK\")
                  (princ \"FAIL\"))))" 2>/dev/null)
  if [ "$result" = "FAIL" ]; then
    echo "FAIL: $f has indentation issues"
    FAILED=1
  fi
done

if [ "$FAILED" -eq 1 ]; then
  echo ""
  echo "Fix indentation with: emacs -batch -L . --eval '(progn (find-file \"FILE\") (indent-region (point-min) (point-max)) (save-buffer))'"
  exit 1
fi

echo "All ${#ALL_EL_FILES[@]} files properly indented."
