;; test/test-setup.el — loaded before tests

(when (require 'undercover nil t)
  (undercover "*.el"
              (:exclude "*-test.el" "*-test-setup.el"
                        "*-benchmark.el" "*-examples.el")
              (:report-format 'lcov)
              (:report-file "lcov.info")
              (:send-report nil)
              (:merge-report nil)))
