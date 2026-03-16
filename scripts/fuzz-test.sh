#!/usr/bin/env bash
set -euo pipefail

# Fuzz test the vCard parser with random and malformed inputs.
# Verifies the parser doesn't crash on unexpected data.

cd "$(git rev-parse --show-toplevel)"

NUM_CASES=${NUM_CASES:-1000}

echo "Running fuzz tests ($NUM_CASES cases)..."
emacs -batch -L . -l ecard.el \
  --eval "
(progn
  (random t)

  (defun ecard--fuzz-random-string (len)
    \"Generate a random printable ASCII string of LEN characters.\"
    (let ((s (make-string len 0)))
      (dotimes (i len s)
        (aset s i (+ 32 (random 95))))))

  (defun ecard--fuzz-random-bytes (len)
    \"Generate random bytes including non-ASCII.\"
    (let ((s (make-string len 0)))
      (dotimes (i len s)
        (aset s i (random 256)))))

  (defun ecard--fuzz-valid-vcard ()
    \"Generate a semi-valid vCard with random content.\"
    (concat \"BEGIN:VCARD\nVERSION:4.0\nFN:\"
            (ecard--fuzz-random-string (1+ (random 50)))
            \"\n\"
            (mapconcat
             (lambda (_)
               (let ((prop (nth (random 10)
                                '(\"EMAIL\" \"TEL\" \"ORG\" \"NOTE\" \"ADR\"
                                  \"TITLE\" \"URL\" \"NICKNAME\" \"BDAY\" \"GEO\"))))
                 (concat prop \":\" (ecard--fuzz-random-string (1+ (random 100))))))
             (make-list (random 20) nil)
             \"\n\")
            \"\nEND:VCARD\"))

  (defun ecard--fuzz-malformed-vcard ()
    \"Generate a malformed vCard.\"
    (let ((variant (random 8)))
      (cond
       ;; Missing BEGIN
       ((= variant 0) (concat \"VERSION:4.0\nFN:Test\nEND:VCARD\"))
       ;; Missing END
       ((= variant 1) (concat \"BEGIN:VCARD\nVERSION:4.0\nFN:Test\"))
       ;; Missing VERSION
       ((= variant 2) (concat \"BEGIN:VCARD\nFN:Test\nEND:VCARD\"))
       ;; Wrong VERSION
       ((= variant 3) (concat \"BEGIN:VCARD\nVERSION:3.0\nFN:Test\nEND:VCARD\"))
       ;; Completely random bytes
       ((= variant 4) (ecard--fuzz-random-bytes (1+ (random 500))))
       ;; Empty string
       ((= variant 5) \"\")
       ;; Very long lines (10K+ characters)
       ((= variant 6)
        (concat \"BEGIN:VCARD\nVERSION:4.0\nFN:\"
                (ecard--fuzz-random-string 10000)
                \"\nEND:VCARD\"))
       ;; Nested BEGIN:VCARD
       ((= variant 7)
        (concat \"BEGIN:VCARD\nVERSION:4.0\nFN:Outer\n\"
                \"BEGIN:VCARD\nVERSION:4.0\nFN:Inner\nEND:VCARD\n\"
                \"END:VCARD\")))))

  (let ((failures 0)
        (total $NUM_CASES)
        (unexpected-errors '()))
    ;; Half valid, half malformed
    (dotimes (i total)
      (let ((input (if (< (random 2) 1)
                       (ecard--fuzz-valid-vcard)
                     (ecard--fuzz-malformed-vcard))))
        (condition-case err
            (ecard-parse input)
          (ecard-parse-error nil)
          (ecard-validation-error nil)
          (error
           (setq failures (1+ failures))
           (push (format \"Case %d: %S\" i (car err)) unexpected-errors)))))

    (message \"Fuzz testing complete: %d/%d passed\"
             (- total failures) total)

    (when (> failures 0)
      (message \"Unexpected errors (%d):\" failures)
      (dolist (e (seq-take (nreverse unexpected-errors) 10))
        (message \"  %s\" e))
      (kill-emacs 1))

    (message \"All %d fuzz test cases handled correctly.\" total)))"
