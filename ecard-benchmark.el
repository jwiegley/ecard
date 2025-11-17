;;; ecard-benchmark.el --- Performance benchmarks for vCard parsing -*- lexical-binding: t; -*-

;; Copyright (C) 2025 John Wiegley

;; Author: John Wiegley <johnw@gnu.org>

;;; Commentary:

;; Benchmarks to measure vCard parsing performance improvements.
;; Run with: emacs -batch -L . -l ecard.el -l ecard-compat.el -l ecard-benchmark.el -f ecard-benchmark-run-all

;;; Code:

(require 'ecard)
(require 'ecard-compat)

(defun ecard-benchmark--generate-test-ecard (index)
  "Generate a test vCard with INDEX."
  (format "BEGIN:VCARD
VERSION:3.0
FN:Contact %d
N:Contact%d;Test;Middle;Dr.;Jr.
ORG:Example Corp;Engineering;Team %d
TITLE:Software Engineer %d
TEL;TYPE=HOME,VOICE:555-1%03d
TEL;TYPE=WORK,FAX:555-2%03d
EMAIL;TYPE=INTERNET:contact%d@example.com
EMAIL;TYPE=WORK:work%d@example.com
ADR;TYPE=HOME:;;123 Main St %d;Springfield;IL;62701;USA
ADR;TYPE=WORK:;;456 Work Ave %d;Chicago;IL;60601;USA
NOTE:This is a test note for contact %d with some\\nline breaks\\nand multiple lines.
BDAY:1980-01-15
URL:http://example.com/contact%d
CATEGORIES:Work,Friend,VIP
PHOTO;ENCODING=BASE64;TYPE=JPEG:SGVsbG8gV29ybGQ=
END:VCARD"
          index index index index index index index index index index index index))

(defun ecard-benchmark--generate-test-file (num-vcards)
  "Generate a test file content with NUM-VCARDS vCards."
  (let ((vcards nil))
    (dotimes (i num-vcards)
      (push (ecard-benchmark--generate-test-ecard (1+ i)) vcards))
    (mapconcat #'identity (nreverse vcards) "\n")))

(defun ecard-benchmark-parse-large-file (&optional num-vcards iterations)
  "Benchmark parsing a file with NUM-VCARDS vCards, ITERATIONS times.
NUM-VCARDS defaults to 100, ITERATIONS defaults to 10."
  (interactive)
  (let* ((num-vcards (or num-vcards 100))
         (iterations (or iterations 10))
         (test-data (ecard-benchmark--generate-test-file num-vcards))
         (data-size (length test-data))
         (start-time (current-time))
         (results nil))

    (message "Benchmarking vCard parsing:")
    (message "  Test data: %d vCards, %d bytes (%.1f KB)"
             num-vcards data-size (/ data-size 1024.0))
    (message "  Running %d iterations..." iterations)

    (dotimes (i iterations)
      (let ((iter-start (current-time)))
        (setq results (ecard-compat-parse-multiple test-data))
        (let ((elapsed (float-time (time-subtract (current-time) iter-start))))
          (when (zerop (mod i (max 1 (/ iterations 10))))
            (message "    Iteration %d/%d: %.3f seconds" (1+ i) iterations elapsed)))))

    (let* ((total-time (float-time (time-subtract (current-time) start-time)))
           (avg-time (/ total-time iterations))
           (vcards-per-sec (/ num-vcards avg-time))
           (bytes-per-sec (/ data-size avg-time)))

      (message "\nResults:")
      (message "  Total time: %.3f seconds" total-time)
      (message "  Average time per iteration: %.3f seconds" avg-time)
      (message "  Throughput: %.1f vCards/sec, %.1f KB/sec"
               vcards-per-sec (/ bytes-per-sec 1024.0))
      (message "  Parsed %d vCards successfully" (length results))

      (list :total-time total-time
            :avg-time avg-time
            :vcards-per-sec vcards-per-sec
            :bytes-per-sec bytes-per-sec
            :num-vcards (length results)))))

(defun ecard-benchmark-parse-realistic (&optional iterations)
  "Benchmark parsing realistic vCard counts (100, 500, 1600).
Simulates the user's use case with 1600 vCards in 135KB file.
ITERATIONS defaults to 5 for large counts."
  (interactive)
  (let ((iterations (or iterations 5)))
    (message "\n=== vCard Performance Benchmark ===\n")

    ;; Small file (100 vCards)
    (message "--- Small file (100 vCards) ---")
    (ecard-benchmark-parse-large-file 100 (* iterations 2))

    ;; Medium file (500 vCards)
    (message "\n--- Medium file (500 vCards) ---")
    (ecard-benchmark-parse-large-file 500 iterations)

    ;; Large file (1600 vCards - user's scenario)
    (message "\n--- Large file (1600 vCards - user scenario) ---")
    (ecard-benchmark-parse-large-file 1600 iterations)

    (message "\n=== Benchmark complete ===\n")))

(defun ecard-benchmark-compare-line-building ()
  "Benchmark line unfolding performance (test `ecard--unfold-lines' optimization)."
  (interactive)
  (let* ((long-line (concat "LONG-PROPERTY:This is a very long value that will be folded "
                           (make-string 1000 ?x)))
         (folded-ecard (format "BEGIN:VCARD\nVERSION:4.0\nFN:Test\n%s\nEND:VCARD" long-line))
         (iterations 1000)
         (start-time (current-time)))

    (message "\nBenchmarking line unfolding:")
    (message "  Test: Parsing vCard with very long line")
    (message "  Iterations: %d" iterations)

    (dotimes (_ iterations)
      (ecard--unfold-lines folded-ecard))

    (let* ((elapsed (float-time (time-subtract (current-time) start-time)))
           (per-iteration (/ elapsed iterations)))

      (message "Results:")
      (message "  Total time: %.3f seconds" elapsed)
      (message "  Average per iteration: %.6f seconds" per-iteration)
      (message "  Throughput: %.1f unfolds/sec" (/ iterations elapsed)))))

(defun ecard-benchmark-run-all ()
  "Run all vCard benchmarks."
  (interactive)
  (message "\n╔════════════════════════════════════════════════════════════╗")
  (message "║         vCard Performance Optimization Benchmark          ║")
  (message "╚════════════════════════════════════════════════════════════╝\n")

  (ecard-benchmark-parse-realistic 5)
  (ecard-benchmark-compare-line-building)

  (message "\n╔════════════════════════════════════════════════════════════╗")
  (message "║                    All Benchmarks Complete                 ║")
  (message "╚════════════════════════════════════════════════════════════╝\n"))

(provide 'ecard-benchmark)
;;; ecard-benchmark.el ends here
