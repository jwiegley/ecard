;;; ecard-carddav-map-test.el --- Tests for CardDAV map operations -*- lexical-binding: t; -*-

;; Copyright (C) 2025 John Wiegley

;; Author: John Wiegley <johnw@gnu.org>
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: comm, data, carddav, testing

;;; Commentary:

;; Comprehensive test suite for ecard-carddav-map module.
;; Uses mock CardDAV server to avoid network dependencies.
;;
;; Run tests with:
;;   emacs -batch -L . -l ecard.el -l ecard-compat.el \
;;         -l ecard-carddav-auth.el -l ecard-carddav.el \
;;         -l ecard-carddav-mock.el -l ecard-carddav-map.el \
;;         -l ecard-carddav-map-test.el \
;;         -f ert-run-tests-batch-and-exit
;;
;; Or interactively:
;;   M-x load-file RET ecard-carddav-map-test.el RET
;;   M-x ert RET t RET

;;; Code:

(require 'ert)
(require 'ecard)
(require 'ecard-carddav-auth)
(require 'ecard-carddav)
(require 'ecard-carddav-mock)
(require 'ecard-carddav-map)

;;; Test fixtures

(defvar ecard-carddav-map-test--mock-server nil
  "Mock server for testing.")

(defvar ecard-carddav-map-test--addressbook nil
  "Test addressbook.")

(defun ecard-carddav-map-test--setup ()
  "Set up test fixtures."
  ;; Create mock server
  (setq ecard-carddav-map-test--mock-server
        (ecard-carddav-mock-server-create
         :base-url "https://test.example.com"))

  ;; Add address book
  (ecard-carddav-mock-add-addressbook
   ecard-carddav-map-test--mock-server
   "/addressbooks/user/contacts/"
   "Test Contacts"
   "Test address book")

  ;; Install mock
  (ecard-carddav-mock-install ecard-carddav-map-test--mock-server)

  ;; Get addressbook reference
  (let* ((auth (ecard-carddav-auth-basic-create
                :username "user"
                :password "pass"))
         (server (ecard-carddav-server-create
                  :url "https://test.example.com"
                  :auth auth))
         (addressbooks (ecard-carddav-discover-addressbooks server)))
    (setq ecard-carddav-map-test--addressbook (car addressbooks))))

(defun ecard-carddav-map-test--teardown ()
  "Tear down test fixtures."
  ;; Uninstall mock
  (ecard-carddav-mock-uninstall)

  (setq ecard-carddav-map-test--mock-server nil
        ecard-carddav-map-test--addressbook nil))

(defun ecard-carddav-map-test--create-test-ecard (fn &optional note)
  "Create a test vCard with formatted name FN and optional NOTE."
  (let ((card (ecard-create
               :fn fn
               :n (list "Doe" "John" "" "" "")
               :email "john@example.com"
               :tel "+1-555-1234"
               :uid (format "test-%s@example.com"
                           (downcase (replace-regexp-in-string " " "-" fn))))))
    (when note
      (ecard-add-property card 'note note))
    card))

(defun ecard-carddav-map-test--populate-addressbook (count)
  "Add COUNT test vCards to addressbook."
  (dotimes (i count)
    (let ((name (format "Contact %d" (1+ i)))
          (path (format "/addressbooks/user/contacts/contact%d.vcf" (1+ i))))
      (ecard-carddav-put-ecard
       ecard-carddav-map-test--addressbook
       path
       (ecard-carddav-map-test--create-test-ecard name)))))

;;; Basic functionality tests

(ert-deftest ecard-carddav-map-test-empty-addressbook ()
  "Test mapping over empty addressbook."
  (ecard-carddav-map-test--setup)
  (unwind-protect
      (let* ((call-count 0)
             (result (ecard-carddav-map-resources
                     ecard-carddav-map-test--addressbook
                     (lambda (_resource)
                       (setq call-count (1+ call-count))
                       nil))))
        (should (= call-count 0))
        (should (= (plist-get result :total) 0))
        (should (= (plist-get result :processed) 0))
        (should (= (plist-get result :modified) 0))
        (should (= (plist-get result :deleted) 0))
        (should (= (plist-get result :failed) 0))
        (should (= (plist-get result :skipped) 0)))
    (ecard-carddav-map-test--teardown)))

(ert-deftest ecard-carddav-map-test-no-modifications ()
  "Test mapping that modifies no resources."
  (ecard-carddav-map-test--setup)
  (unwind-protect
      (progn
        (ecard-carddav-map-test--populate-addressbook 5)
        (let* ((call-count 0)
               (result (ecard-carddav-map-resources
                       ecard-carddav-map-test--addressbook
                       (lambda (_resource)
                         (setq call-count (1+ call-count))
                         nil))))  ; Return nil - no modification
          (should (= call-count 5))
          (should (= (plist-get result :total) 5))
          (should (= (plist-get result :processed) 5))
          (should (= (plist-get result :modified) 0))
          (should (= (plist-get result :deleted) 0))
          (should (= (plist-get result :failed) 0))))
    (ecard-carddav-map-test--teardown)))

(ert-deftest ecard-carddav-map-test-modify-all-resources ()
  "Test mapping that modifies all resources."
  (ecard-carddav-map-test--setup)
  (unwind-protect
      (progn
        (ecard-carddav-map-test--populate-addressbook 3)
        (let ((result (ecard-carddav-map-resources
                      ecard-carddav-map-test--addressbook
                      (lambda (resource)
                        (let ((ecard (oref resource ecard)))
                          (ecard-add-property ecard 'note "Test note")
                          t)))))  ; Return t - modified
          (should (= (plist-get result :total) 3))
          (should (= (plist-get result :processed) 3))
          (should (= (plist-get result :modified) 3))
          (should (= (plist-get result :deleted) 0))
          (should (= (plist-get result :failed) 0))

          ;; Verify modifications were saved
          (let ((resources (ecard-carddav-list-resources
                           ecard-carddav-map-test--addressbook)))
            (dolist (resource-info resources)
              (let ((resource (ecard-carddav-get-resource
                              ecard-carddav-map-test--addressbook
                              (oref resource-info path))))
                (should (oref (oref resource ecard) note)))))))
    (ecard-carddav-map-test--teardown)))

(ert-deftest ecard-carddav-map-test-modify-some-resources ()
  "Test mapping that modifies only some resources."
  (ecard-carddav-map-test--setup)
  (unwind-protect
      (progn
        (ecard-carddav-map-test--populate-addressbook 6)
        (let ((result (ecard-carddav-map-resources
                      ecard-carddav-map-test--addressbook
                      (lambda (resource)
                        (let* ((ecard (oref resource ecard))
                               (fn (ecard-get-property-value ecard 'fn)))
                          ;; Only modify even-numbered contacts
                          (when (string-match "Contact \\([0-9]+\\)" fn)
                            (let ((num (string-to-number (match-string 1 fn))))
                              (when (zerop (mod num 2))
                                (ecard-add-property ecard 'note "Even contact")
                                t))))))))
          (should (= (plist-get result :total) 6))
          (should (= (plist-get result :processed) 6))
          (should (= (plist-get result :modified) 3))  ; Contacts 2, 4, 6
          (should (= (plist-get result :deleted) 0))
          (should (= (plist-get result :failed) 0))))
    (ecard-carddav-map-test--teardown)))

(ert-deftest ecard-carddav-map-test-delete-resources ()
  "Test mapping that deletes resources."
  (ecard-carddav-map-test--setup)
  (unwind-protect
      (progn
        (ecard-carddav-map-test--populate-addressbook 4)
        (let ((result (ecard-carddav-map-resources
                      ecard-carddav-map-test--addressbook
                      (lambda (resource)
                        (let* ((ecard (oref resource ecard))
                               (fn (ecard-get-property-value ecard 'fn)))
                          ;; Delete contacts with odd numbers
                          (when (string-match "Contact \\([0-9]+\\)" fn)
                            (let ((num (string-to-number (match-string 1 fn))))
                              (when (not (zerop (mod num 2)))
                                :delete))))))))
          (should (= (plist-get result :total) 4))
          (should (= (plist-get result :processed) 4))
          (should (= (plist-get result :modified) 0))
          (should (= (plist-get result :deleted) 2))  ; Contacts 1, 3
          (should (= (plist-get result :failed) 0))

          ;; Verify deletions
          (let ((resources (ecard-carddav-list-resources
                           ecard-carddav-map-test--addressbook)))
            (should (= (length resources) 2))
            ;; Remaining should be contacts 2 and 4
            (let ((paths (mapcar (lambda (r) (oref r path)) resources)))
              (should (member "/addressbooks/user/contacts/contact2.vcf" paths))
              (should (member "/addressbooks/user/contacts/contact4.vcf" paths))))))
    (ecard-carddav-map-test--teardown)))

(ert-deftest ecard-carddav-map-test-skip-resources ()
  "Test mapping that explicitly skips resources."
  (ecard-carddav-map-test--setup)
  (unwind-protect
      (progn
        (ecard-carddav-map-test--populate-addressbook 5)
        (let ((result (ecard-carddav-map-resources
                      ecard-carddav-map-test--addressbook
                      (lambda (resource)
                        (let* ((ecard (oref resource ecard))
                               (fn (ecard-get-property-value ecard 'fn)))
                          ;; Skip first 3 contacts
                          (if (string-match "Contact [123]" fn)
                              :skip
                            (ecard-add-property ecard 'note "Processed")
                            t))))))
          (should (= (plist-get result :total) 5))
          (should (= (plist-get result :processed) 5))
          (should (= (plist-get result :modified) 2))  ; Contacts 4, 5
          (should (= (plist-get result :skipped) 3))  ; Contacts 1, 2, 3
          (should (= (plist-get result :failed) 0))))
    (ecard-carddav-map-test--teardown)))

;;; Change detection tests

(ert-deftest ecard-carddav-map-test-false-modification-claim ()
  "Test that returning t without actual change doesn't update server."
  (ecard-carddav-map-test--setup)
  (unwind-protect
      (progn
        (ecard-carddav-map-test--populate-addressbook 3)
        (let ((result (ecard-carddav-map-resources
                      ecard-carddav-map-test--addressbook
                      (lambda (_resource)
                        ;; Claim modification but don't actually modify
                        t))))
          (should (= (plist-get result :total) 3))
          (should (= (plist-get result :processed) 3))
          ;; Should be 0 because change detection detects no actual change
          (should (= (plist-get result :modified) 0))
          (should (= (plist-get result :skipped) 3))))  ; Skipped due to no change
    (ecard-carddav-map-test--teardown)))

(ert-deftest ecard-carddav-map-test-actual-modification-detected ()
  "Test that actual modifications are correctly detected."
  (ecard-carddav-map-test--setup)
  (unwind-protect
      (progn
        (ecard-carddav-map-test--populate-addressbook 2)
        (let ((result (ecard-carddav-map-resources
                      ecard-carddav-map-test--addressbook
                      (lambda (resource)
                        (let ((ecard (oref resource ecard)))
                          ;; Make actual modification
                          (ecard-add-property ecard 'note "Modified")
                          t)))))
          (should (= (plist-get result :modified) 2))
          (should (= (plist-get result :skipped) 0))))
    (ecard-carddav-map-test--teardown)))

;;; Error handling tests

(ert-deftest ecard-carddav-map-test-transformation-error ()
  "Test handling of errors in transformation function."
  (ecard-carddav-map-test--setup)
  (unwind-protect
      (progn
        (ecard-carddav-map-test--populate-addressbook 3)
        (let* ((processed-paths nil)
               (result (ecard-carddav-map-resources
                       ecard-carddav-map-test--addressbook
                       (lambda (resource)
                         (let ((path (oref resource path)))
                           (push path processed-paths)
                           ;; Throw error on second resource processed
                           (when (= (length processed-paths) 2)
                             (error "Test error"))
                           nil)))))
          (should (= (plist-get result :total) 3))
          (should (= (plist-get result :processed) 3))
          ;; Should have at least 1 error (the one we threw)
          (should (>= (plist-get result :failed) 1))
          (should (>= (length (plist-get result :errors)) 1))
          ;; Verify at least one error has the right type
          (should (cl-some (lambda (err)
                            (eq (plist-get err :type) 'transform-error))
                          (plist-get result :errors)))))
    (ecard-carddav-map-test--teardown)))

(ert-deftest ecard-carddav-map-test-etag-conflict-skip ()
  "Test ETag conflict handling with :skip strategy."
  (ecard-carddav-map-test--setup)
  (unwind-protect
      (progn
        (ecard-carddav-map-test--populate-addressbook 1)

        ;; Simulate concurrent modification by updating the resource
        ;; after we've fetched it
        (let ((result (ecard-carddav-map-resources
                      ecard-carddav-map-test--addressbook
                      (lambda (resource)
                        (let ((ecard (oref resource ecard)))
                          ;; Simulate someone else updating the resource
                          ;; by invalidating the ETag
                          (oset resource etag "invalid-etag")
                          (ecard-add-property ecard 'note "Conflict test")
                          t))
                      :on-conflict :skip)))
          (should (= (plist-get result :total) 1))
          (should (= (plist-get result :processed) 1))
          (should (= (plist-get result :modified) 0))
          (should (= (plist-get result :skipped) 1))
          ;; Should have conflict error
          (should (= (length (plist-get result :errors)) 1))
          (let ((err (car (plist-get result :errors))))
            (should (eq (plist-get err :type) 'conflict)))))
    (ecard-carddav-map-test--teardown)))

(ert-deftest ecard-carddav-map-test-etag-conflict-force ()
  "Test ETag conflict handling with :force strategy."
  (ecard-carddav-map-test--setup)
  (unwind-protect
      (progn
        ;; Create a contact
        (ecard-carddav-put-ecard
         ecard-carddav-map-test--addressbook
         "/addressbooks/user/contacts/test.vcf"
         (ecard-carddav-map-test--create-test-ecard "Test User"))

        ;; Map with force conflict resolution
        (let ((result (ecard-carddav-map-resources
                      ecard-carddav-map-test--addressbook
                      (lambda (resource)
                        ;; Modify the contact
                        (let ((ecard (oref resource ecard)))
                          (ecard-add-property ecard 'note "Force update")
                          t))
                      :on-conflict :force)))
          (should (= (plist-get result :total) 1))
          (should (= (plist-get result :processed) 1))
          (should (= (plist-get result :modified) 1))))
    (ecard-carddav-map-test--teardown)))

(ert-deftest ecard-carddav-map-test-delete-conflict-skip ()
  "Test delete conflict handling with :skip strategy."
  (ecard-carddav-map-test--setup)
  (unwind-protect
      (progn
        (ecard-carddav-map-test--populate-addressbook 1)

        (let ((result (ecard-carddav-map-resources
                      ecard-carddav-map-test--addressbook
                      (lambda (resource)
                        ;; Invalidate ETag before delete
                        (oset resource etag "invalid-etag")
                        :delete)
                      :on-conflict :skip)))
          (should (= (plist-get result :total) 1))
          (should (= (plist-get result :processed) 1))
          (should (= (plist-get result :deleted) 0))
          (should (= (plist-get result :skipped) 1))
          (should (= (length (plist-get result :errors)) 1))))
    (ecard-carddav-map-test--teardown)))

(ert-deftest ecard-carddav-map-test-delete-conflict-force ()
  "Test delete conflict handling with :force strategy."
  (ecard-carddav-map-test--setup)
  (unwind-protect
      (progn
        (ecard-carddav-put-ecard
         ecard-carddav-map-test--addressbook
         "/addressbooks/user/contacts/test.vcf"
         (ecard-carddav-map-test--create-test-ecard "Test User"))

        (let ((result (ecard-carddav-map-resources
                      ecard-carddav-map-test--addressbook
                      (lambda (_resource)
                        :delete)
                      :on-conflict :force)))
          (should (= (plist-get result :total) 1))
          (should (= (plist-get result :processed) 1))
          (should (= (plist-get result :deleted) 1))

          ;; Verify resource was deleted
          (should-error
           (ecard-carddav-get-resource
            ecard-carddav-map-test--addressbook
            "/addressbooks/user/contacts/test.vcf")
           :type 'ecard-carddav-not-found-error)))
    (ecard-carddav-map-test--teardown)))

;;; Progress reporting tests

(ert-deftest ecard-carddav-map-test-progress-callback ()
  "Test progress callback is called with correct statistics."
  (ecard-carddav-map-test--setup)
  (unwind-protect
      (progn
        (ecard-carddav-map-test--populate-addressbook 10)

        (let ((callback-calls nil))
          (ecard-carddav-map-resources
           ecard-carddav-map-test--addressbook
           (lambda (resource)
             (let ((ecard (oref resource ecard)))
               (ecard-add-property ecard 'note "Progress test")
               t))
           :progress-callback
           (lambda (stats)
             (push (copy-sequence stats) callback-calls)))

          ;; Should be called periodically (every 5 resources) plus final call
          (should (>= (length callback-calls) 3))

          ;; Verify statistics are accurate in final call
          (let ((final-stats (car callback-calls)))
            (should (= (plist-get final-stats :total) 10))
            (should (= (plist-get final-stats :processed) 10))
            (should (= (plist-get final-stats :modified) 10)))))
    (ecard-carddav-map-test--teardown)))

(ert-deftest ecard-carddav-map-test-progress-incremental ()
  "Test progress callback shows incremental progress."
  (ecard-carddav-map-test--setup)
  (unwind-protect
      (progn
        (ecard-carddav-map-test--populate-addressbook 15)

        (let ((callback-calls nil))
          (ecard-carddav-map-resources
           ecard-carddav-map-test--addressbook
           (lambda (_resource) nil)
           :progress-callback
           (lambda (stats)
             (push (plist-get stats :processed) callback-calls)))

          ;; Verify we got progress callbacks
          (should (>= (length callback-calls) 3))  ; At least 3 calls (5, 10, 15)

          ;; Verify final callback shows all processed
          (should (= (car callback-calls) 15))

          ;; Verify processed count never decreases
          (setq callback-calls (nreverse callback-calls))
          (let ((prev 0))
            (dolist (processed callback-calls)
              (should (>= processed prev))
              (setq prev processed)))))
    (ecard-carddav-map-test--teardown)))

;;; Batch processing tests

(ert-deftest ecard-carddav-map-test-custom-batch-size ()
  "Test custom batch size parameter."
  (ecard-carddav-map-test--setup)
  (unwind-protect
      (progn
        (ecard-carddav-map-test--populate-addressbook 20)

        (let ((result (ecard-carddav-map-resources
                      ecard-carddav-map-test--addressbook
                      (lambda (resource)
                        (let ((ecard (oref resource ecard)))
                          (ecard-add-property ecard 'note "Batch test")
                          t))
                      :batch-size 5)))  ; Process in batches of 5
          (should (= (plist-get result :total) 20))
          (should (= (plist-get result :processed) 20))
          (should (= (plist-get result :modified) 20))))
    (ecard-carddav-map-test--teardown)))

(ert-deftest ecard-carddav-map-test-large-addressbook ()
  "Test handling of large addressbook."
  (ecard-carddav-map-test--setup)
  (unwind-protect
      (progn
        (ecard-carddav-map-test--populate-addressbook 50)

        (let ((result (ecard-carddav-map-resources
                      ecard-carddav-map-test--addressbook
                      (lambda (resource)
                        ;; Modify every 5th contact
                        (let* ((ecard (oref resource ecard))
                               (fn (ecard-get-property-value ecard 'fn)))
                          (when (string-match "Contact \\([0-9]+\\)" fn)
                            (let ((num (string-to-number (match-string 1 fn))))
                              (when (zerop (mod num 5))
                                (ecard-add-property ecard 'note "Large test")
                                t))))))))
          (should (= (plist-get result :total) 50))
          (should (= (plist-get result :processed) 50))
          (should (= (plist-get result :modified) 10))))  ; 10 multiples of 5
    (ecard-carddav-map-test--teardown)))

;;; Filter function tests

(ert-deftest ecard-carddav-map-test-filter-function ()
  "Test filter function to process subset of resources."
  (ecard-carddav-map-test--setup)
  (unwind-protect
      (progn
        (ecard-carddav-map-test--populate-addressbook 10)

        (let ((result (ecard-carddav-map-resources
                      ecard-carddav-map-test--addressbook
                      (lambda (resource)
                        (let ((ecard (oref resource ecard)))
                          (ecard-add-property ecard 'note "Filtered")
                          t))
                      :filter-fn
                      (lambda (path)
                        ;; Only process contacts 1-5
                        (string-match "contact[1-5]\\.vcf$" path)))))
          (should (= (plist-get result :total) 5))  ; Filtered to 5
          (should (= (plist-get result :processed) 5))
          (should (= (plist-get result :modified) 5))))
    (ecard-carddav-map-test--teardown)))

;;; Mixed operation tests

(ert-deftest ecard-carddav-map-test-mixed-operations ()
  "Test mix of modifications, deletions, and skips."
  (ecard-carddav-map-test--setup)
  (unwind-protect
      (progn
        (ecard-carddav-map-test--populate-addressbook 12)

        (let ((result (ecard-carddav-map-resources
                      ecard-carddav-map-test--addressbook
                      (lambda (resource)
                        (let* ((ecard (oref resource ecard))
                               (fn (ecard-get-property-value ecard 'fn))
                               (num (when (string-match "Contact \\([0-9]+\\)" fn)
                                      (string-to-number (match-string 1 fn)))))
                          (cond
                           ;; Delete multiples of 3
                           ((zerop (mod num 3)) :delete)
                           ;; Skip multiples of 2 (but not 3)
                           ((zerop (mod num 2)) :skip)
                           ;; Modify others (1, 5, 7, 11)
                           (t
                            (ecard-add-property ecard 'note "Modified")
                            t)))))))
          (should (= (plist-get result :total) 12))
          (should (= (plist-get result :processed) 12))
          (should (= (plist-get result :modified) 4))   ; 1, 5, 7, 11
          (should (= (plist-get result :deleted) 4))    ; 3, 6, 9, 12
          (should (= (plist-get result :skipped) 4))))  ; 2, 4, 8, 10
    (ecard-carddav-map-test--teardown)))

;;; Idempotency tests

(ert-deftest ecard-carddav-map-test-idempotent-transformation ()
  "Test running same transformation twice produces correct results."
  (ecard-carddav-map-test--setup)
  (unwind-protect
      (progn
        (ecard-carddav-map-test--populate-addressbook 5)

        ;; First run - add note
        (let ((result1 (ecard-carddav-map-resources
                       ecard-carddav-map-test--addressbook
                       (lambda (resource)
                         (let ((ecard (oref resource ecard)))
                           (unless (ecard-note ecard)
                             (ecard-add-property ecard 'note "First run")
                             t))))))
          (should (= (plist-get result1 :modified) 5))

          ;; Second run - should not modify anything
          (let ((result2 (ecard-carddav-map-resources
                         ecard-carddav-map-test--addressbook
                         (lambda (resource)
                           (let ((ecard (oref resource ecard)))
                             (unless (ecard-note ecard)
                               (ecard-add-property ecard 'note "First run")
                               t))))))
            ;; Second run should have 0 modifications because notes already exist
            (should (= (plist-get result2 :modified) 0))
            ;; No resources are explicitly skipped - function returns nil
            ;; which is "no change", not :skip
            (should (= (plist-get result2 :skipped) 0)))))
    (ecard-carddav-map-test--teardown)))

(ert-deftest ecard-carddav-map-test-multiple-runs-different-transformations ()
  "Test running different transformations sequentially."
  (ecard-carddav-map-test--setup)
  (unwind-protect
      (progn
        (ecard-carddav-map-test--populate-addressbook 3)

        ;; First transformation - add note
        (ecard-carddav-map-resources
         ecard-carddav-map-test--addressbook
         (lambda (resource)
           (let ((ecard (oref resource ecard)))
             (ecard-add-property ecard 'note "First")
             t)))

        ;; Second transformation - add title
        (let ((result (ecard-carddav-map-resources
                      ecard-carddav-map-test--addressbook
                      (lambda (resource)
                        (let ((ecard (oref resource ecard)))
                          (ecard-add-property ecard 'title "Manager")
                          t)))))
          (should (= (plist-get result :modified) 3))

          ;; Verify both properties exist
          (let ((test-resource (ecard-carddav-get-resource
                               ecard-carddav-map-test--addressbook
                               "/addressbooks/user/contacts/contact1.vcf")))
            (should (oref (oref test-resource ecard) note))
            (should (oref (oref test-resource ecard) title)))))
    (ecard-carddav-map-test--teardown)))

;;; Real-world usage tests

(ert-deftest ecard-carddav-map-test-add-missing-uid ()
  "Test adding UID to contacts that lack one."
  (ecard-carddav-map-test--setup)
  (unwind-protect
      (progn
        ;; Create contacts, some with UID, some without
        (dotimes (i 5)
          (let* ((name (format "Contact %d" (1+ i)))
                 (path (format "/addressbooks/user/contacts/contact%d.vcf" (1+ i)))
                 (card (ecard-create :fn name :email "test@example.com")))
            ;; Only add UID to odd-numbered contacts
            (when (= 0 (mod i 2))
              (ecard-add-property card 'uid (format "uuid-%d" i)))
            (ecard-carddav-put-ecard ecard-carddav-map-test--addressbook path card)))

        ;; Add UID to contacts that don't have one
        (let ((result (ecard-carddav-map-resources
                      ecard-carddav-map-test--addressbook
                      (lambda (resource)
                        (let ((ecard (oref resource ecard)))
                          (unless (ecard-uid ecard)
                            (ecard-add-property ecard 'uid
                                               (format "generated-%s"
                                                      (md5 (ecard-get-property-value ecard 'fn))))
                            t))))))
          (should (= (plist-get result :total) 5))
          (should (= (plist-get result :modified) 2))  ; 2 even-numbered contacts

          ;; Verify all contacts now have UID
          (let ((resources (ecard-carddav-list-resources
                           ecard-carddav-map-test--addressbook)))
            (dolist (resource-info resources)
              (let ((resource (ecard-carddav-get-resource
                              ecard-carddav-map-test--addressbook
                              (oref resource-info path))))
                (should (oref (oref resource ecard) uid)))))))
    (ecard-carddav-map-test--teardown)))

(ert-deftest ecard-carddav-map-test-normalize-phone-numbers ()
  "Test normalizing phone number format across all contacts."
  (ecard-carddav-map-test--setup)
  (unwind-protect
      (progn
        ;; Create contacts with various phone formats
        (let ((phones '("+1-555-1234" "555-5678" "(555) 9012" "555.3456")))
          (dotimes (i 4)
            (let ((card (ecard-create
                        :fn (format "Contact %d" (1+ i))
                        :tel (nth i phones))))
              (ecard-carddav-put-ecard
               ecard-carddav-map-test--addressbook
               (format "/addressbooks/user/contacts/contact%d.vcf" (1+ i))
               card))))

        ;; Normalize all phone numbers
        (let ((result (ecard-carddav-map-resources
                      ecard-carddav-map-test--addressbook
                      (lambda (resource)
                        (let ((ecard (oref resource ecard)))
                          (when-let ((tels (ecard-tel ecard)))
                            (dolist (tel tels)
                              ;; Simple normalization: remove all non-digits
                              (let ((value (oref tel value)))
                                (oset tel value
                                     (replace-regexp-in-string "[^0-9]" "" value))))
                            t))))))
          (should (= (plist-get result :modified) 4))

          ;; Verify normalization
          (let ((resource (ecard-carddav-get-resource
                          ecard-carddav-map-test--addressbook
                          "/addressbooks/user/contacts/contact1.vcf")))
            (let* ((ecard (oref resource ecard))
                   (tel (car (ecard-tel ecard))))
              (should (string= (oref tel value) "15551234"))))))
    (ecard-carddav-map-test--teardown)))

(ert-deftest ecard-carddav-map-test-bulk-delete-duplicates ()
  "Test deleting duplicate contacts based on email."
  (ecard-carddav-map-test--setup)
  (unwind-protect
      (progn
        ;; Create contacts with duplicate emails
        (dotimes (i 6)
          (let* ((name (format "Contact %d" (1+ i)))
                 (email (if (< i 3)
                           "duplicate@example.com"
                         (format "unique%d@example.com" i)))
                 (card (ecard-create :fn name :email email)))
            (ecard-carddav-put-ecard
             ecard-carddav-map-test--addressbook
             (format "/addressbooks/user/contacts/contact%d.vcf" (1+ i))
             card)))

        ;; List resources in path order to ensure consistent processing
        (let ((all-resources (ecard-carddav-list-resources
                             ecard-carddav-map-test--addressbook)))
          ;; Sort by path for deterministic processing
          (setq all-resources (sort all-resources
                                   (lambda (a b)
                                     (string< (oref a path) (oref b path)))))

          ;; Manually process in order
          (let ((seen-emails (make-hash-table :test 'equal))
                (delete-count 0))
            (dolist (resource-info all-resources)
              (let* ((resource (ecard-carddav-get-resource
                               ecard-carddav-map-test--addressbook
                               (oref resource-info path)))
                     (ecard (oref resource ecard))
                     (email (ecard-get-property-value ecard 'email)))
                (if (gethash email seen-emails)
                    (progn
                      (ecard-carddav-delete-resource
                       ecard-carddav-map-test--addressbook
                       (oref resource path))
                      (setq delete-count (1+ delete-count)))
                  (puthash email t seen-emails))))

            (should (= delete-count 2))  ; 2 duplicates deleted

            ;; Verify only 4 contacts remain
            (let ((remaining (ecard-carddav-list-resources
                             ecard-carddav-map-test--addressbook)))
              (should (= (length remaining) 4))))))
    (ecard-carddav-map-test--teardown)))

;;; Stats helper tests

(ert-deftest ecard-carddav-map-test-make-stats-defaults ()
  "Test make-stats creates stats with all zero defaults."
  (let ((stats (ecard-carddav-map--make-stats)))
    (should (= (plist-get stats :total) 0))
    (should (= (plist-get stats :processed) 0))
    (should (= (plist-get stats :modified) 0))
    (should (= (plist-get stats :deleted) 0))
    (should (= (plist-get stats :failed) 0))
    (should (= (plist-get stats :skipped) 0))
    (should (null (plist-get stats :errors)))))

(ert-deftest ecard-carddav-map-test-make-stats-with-args ()
  "Test make-stats with keyword arguments."
  (let ((stats (ecard-carddav-map--make-stats :total 10 :modified 3)))
    (should (= (plist-get stats :total) 10))
    (should (= (plist-get stats :modified) 3))
    (should (= (plist-get stats :processed) 0))))

(ert-deftest ecard-carddav-map-test-update-stats ()
  "Test update-stats modifies stats plist."
  (let* ((stats (ecard-carddav-map--make-stats :total 10))
         (updated (ecard-carddav-map--update-stats stats :processed 5 :modified 2)))
    ;; Should return a new copy
    (should (= (plist-get updated :processed) 5))
    (should (= (plist-get updated :modified) 2))
    ;; Original unchanged
    (should (= (plist-get stats :processed) 0))))

(ert-deftest ecard-carddav-map-test-update-stats-add-error ()
  "Test update-stats with :add-error accumulates errors."
  (let* ((stats (ecard-carddav-map--make-stats))
         (err1 (list :path "/a.vcf" :type 'conflict :message "oops"))
         (updated1 (ecard-carddav-map--update-stats stats :add-error err1))
         (err2 (list :path "/b.vcf" :type 'timeout :message "slow"))
         (updated2 (ecard-carddav-map--update-stats updated1 :add-error err2)))
    (should (= (length (plist-get updated1 :errors)) 1))
    (should (= (length (plist-get updated2 :errors)) 2))
    (should (eq (plist-get (car (plist-get updated2 :errors)) :type) 'conflict))
    (should (eq (plist-get (cadr (plist-get updated2 :errors)) :type) 'timeout))))

;;; Handle resource update tests

(ert-deftest ecard-carddav-map-test-handle-resource-update-success ()
  "Test successful resource update increments modified count."
  (ecard-carddav-map-test--setup)
  (unwind-protect
      (progn
        (ecard-carddav-map-test--populate-addressbook 1)
        (let* ((resources (ecard-carddav-list-resources
                          ecard-carddav-map-test--addressbook))
               (resource-info (car resources))
               (resource (ecard-carddav-get-resource
                         ecard-carddav-map-test--addressbook
                         (oref resource-info path)))
               (stats (ecard-carddav-map--make-stats)))
          ;; Modify the ecard
          (ecard-add-property (oref resource ecard) 'note "Updated")
          (let ((result (ecard-carddav-map--handle-resource-update
                        ecard-carddav-map-test--addressbook
                        resource stats :skip)))
            (should (= (plist-get result :modified) 1))
            (should (= (plist-get result :failed) 0)))))
    (ecard-carddav-map-test--teardown)))

(ert-deftest ecard-carddav-map-test-handle-resource-update-conflict-skip ()
  "Test resource update with conflict using skip strategy."
  (ecard-carddav-map-test--setup)
  (unwind-protect
      (progn
        (ecard-carddav-map-test--populate-addressbook 1)
        (let* ((resources (ecard-carddav-list-resources
                          ecard-carddav-map-test--addressbook))
               (resource-info (car resources))
               (resource (ecard-carddav-get-resource
                         ecard-carddav-map-test--addressbook
                         (oref resource-info path)))
               (stats (ecard-carddav-map--make-stats)))
          ;; Invalidate etag to trigger conflict
          (oset resource etag "invalid-etag")
          (ecard-add-property (oref resource ecard) 'note "Updated")
          (let ((result (ecard-carddav-map--handle-resource-update
                        ecard-carddav-map-test--addressbook
                        resource stats :skip)))
            (should (= (plist-get result :modified) 0))
            (should (= (plist-get result :skipped) 1))
            (should (= (length (plist-get result :errors)) 1)))))
    (ecard-carddav-map-test--teardown)))

(ert-deftest ecard-carddav-map-test-handle-resource-update-conflict-retry ()
  "Test resource update with conflict using retry-once strategy."
  (ecard-carddav-map-test--setup)
  (unwind-protect
      (progn
        (ecard-carddav-map-test--populate-addressbook 1)
        (let* ((resources (ecard-carddav-list-resources
                          ecard-carddav-map-test--addressbook))
               (resource-info (car resources))
               (resource (ecard-carddav-get-resource
                         ecard-carddav-map-test--addressbook
                         (oref resource-info path)))
               (stats (ecard-carddav-map--make-stats)))
          ;; Invalidate etag
          (oset resource etag "invalid-etag")
          (ecard-add-property (oref resource ecard) 'note "Retry")
          (let ((result (ecard-carddav-map--handle-resource-update
                        ecard-carddav-map-test--addressbook
                        resource stats :retry-once)))
            ;; Retry should succeed since mock server just re-fetches
            (should (= (plist-get result :modified) 1)))))
    (ecard-carddav-map-test--teardown)))

(ert-deftest ecard-carddav-map-test-handle-resource-update-conflict-force ()
  "Test resource update with conflict using force strategy."
  (ecard-carddav-map-test--setup)
  (unwind-protect
      (progn
        (ecard-carddav-map-test--populate-addressbook 1)
        (let* ((resources (ecard-carddav-list-resources
                          ecard-carddav-map-test--addressbook))
               (resource-info (car resources))
               (resource (ecard-carddav-get-resource
                         ecard-carddav-map-test--addressbook
                         (oref resource-info path)))
               (stats (ecard-carddav-map--make-stats)))
          ;; Invalidate etag
          (oset resource etag "invalid-etag")
          (ecard-add-property (oref resource ecard) 'note "Force")
          (let ((result (ecard-carddav-map--handle-resource-update
                        ecard-carddav-map-test--addressbook
                        resource stats :force)))
            ;; Force should succeed
            (should (= (plist-get result :modified) 1)))))
    (ecard-carddav-map-test--teardown)))

(ert-deftest ecard-carddav-map-test-handle-resource-update-unknown-strategy ()
  "Test resource update conflict with unknown strategy."
  (ecard-carddav-map-test--setup)
  (unwind-protect
      (progn
        (ecard-carddav-map-test--populate-addressbook 1)
        (let* ((resources (ecard-carddav-list-resources
                          ecard-carddav-map-test--addressbook))
               (resource-info (car resources))
               (resource (ecard-carddav-get-resource
                         ecard-carddav-map-test--addressbook
                         (oref resource-info path)))
               (stats (ecard-carddav-map--make-stats)))
          ;; Invalidate etag
          (oset resource etag "invalid-etag")
          (ecard-add-property (oref resource ecard) 'note "Unknown")
          (let ((result (ecard-carddav-map--handle-resource-update
                        ecard-carddav-map-test--addressbook
                        resource stats :bogus)))
            (should (= (plist-get result :failed) 1)))))
    (ecard-carddav-map-test--teardown)))

;;; Handle resource delete tests

(ert-deftest ecard-carddav-map-test-handle-resource-delete-success ()
  "Test successful resource deletion."
  (ecard-carddav-map-test--setup)
  (unwind-protect
      (progn
        (ecard-carddav-map-test--populate-addressbook 1)
        (let* ((resources (ecard-carddav-list-resources
                          ecard-carddav-map-test--addressbook))
               (resource-info (car resources))
               (resource (ecard-carddav-get-resource
                         ecard-carddav-map-test--addressbook
                         (oref resource-info path)))
               (stats (ecard-carddav-map--make-stats)))
          (let ((result (ecard-carddav-map--handle-resource-delete
                        ecard-carddav-map-test--addressbook
                        resource stats :skip)))
            (should (= (plist-get result :deleted) 1))
            (should (= (plist-get result :failed) 0)))))
    (ecard-carddav-map-test--teardown)))

(ert-deftest ecard-carddav-map-test-handle-resource-delete-conflict-skip ()
  "Test resource delete with conflict using skip strategy."
  (ecard-carddav-map-test--setup)
  (unwind-protect
      (progn
        (ecard-carddav-map-test--populate-addressbook 1)
        (let* ((resources (ecard-carddav-list-resources
                          ecard-carddav-map-test--addressbook))
               (resource-info (car resources))
               (resource (ecard-carddav-get-resource
                         ecard-carddav-map-test--addressbook
                         (oref resource-info path)))
               (stats (ecard-carddav-map--make-stats)))
          ;; Invalidate etag
          (oset resource etag "invalid-etag")
          (let ((result (ecard-carddav-map--handle-resource-delete
                        ecard-carddav-map-test--addressbook
                        resource stats :skip)))
            (should (= (plist-get result :deleted) 0))
            (should (= (plist-get result :skipped) 1)))))
    (ecard-carddav-map-test--teardown)))

(ert-deftest ecard-carddav-map-test-handle-resource-delete-conflict-force ()
  "Test resource delete with conflict using force strategy."
  (ecard-carddav-map-test--setup)
  (unwind-protect
      (progn
        (ecard-carddav-map-test--populate-addressbook 1)
        (let* ((resources (ecard-carddav-list-resources
                          ecard-carddav-map-test--addressbook))
               (resource-info (car resources))
               (resource (ecard-carddav-get-resource
                         ecard-carddav-map-test--addressbook
                         (oref resource-info path)))
               (stats (ecard-carddav-map--make-stats)))
          ;; Invalidate etag
          (oset resource etag "invalid-etag")
          (let ((result (ecard-carddav-map--handle-resource-delete
                        ecard-carddav-map-test--addressbook
                        resource stats :force)))
            ;; Force should succeed via re-fetching latest etag
            (should (= (plist-get result :deleted) 1)))))
    (ecard-carddav-map-test--teardown)))

(ert-deftest ecard-carddav-map-test-handle-resource-delete-unknown-strategy ()
  "Test resource delete conflict with unknown strategy."
  (ecard-carddav-map-test--setup)
  (unwind-protect
      (progn
        (ecard-carddav-map-test--populate-addressbook 1)
        (let* ((resources (ecard-carddav-list-resources
                          ecard-carddav-map-test--addressbook))
               (resource-info (car resources))
               (resource (ecard-carddav-get-resource
                         ecard-carddav-map-test--addressbook
                         (oref resource-info path)))
               (stats (ecard-carddav-map--make-stats)))
          ;; Invalidate etag
          (oset resource etag "invalid-etag")
          (let ((result (ecard-carddav-map--handle-resource-delete
                        ecard-carddav-map-test--addressbook
                        resource stats :bogus)))
            (should (= (plist-get result :failed) 1)))))
    (ecard-carddav-map-test--teardown)))

(ert-deftest ecard-carddav-map-test-handle-resource-delete-not-found ()
  "Test deleting already-deleted resource counts as success."
  (ecard-carddav-map-test--setup)
  (unwind-protect
      (progn
        (ecard-carddav-map-test--populate-addressbook 1)
        (let* ((resources (ecard-carddav-list-resources
                          ecard-carddav-map-test--addressbook))
               (resource-info (car resources))
               (resource (ecard-carddav-get-resource
                         ecard-carddav-map-test--addressbook
                         (oref resource-info path)))
               (stats (ecard-carddav-map--make-stats)))
          ;; Delete the resource first
          (ecard-carddav-delete-resource
           ecard-carddav-map-test--addressbook (oref resource path))
          ;; Attempting to delete again should count as deleted (not-found)
          (let ((result (ecard-carddav-map--handle-resource-delete
                        ecard-carddav-map-test--addressbook
                        resource stats :skip)))
            (should (= (plist-get result :deleted) 1)))))
    (ecard-carddav-map-test--teardown)))

;;; Mock server edge case tests

(ert-deftest ecard-carddav-map-test-mock-handle-get-not-found ()
  "Test mock GET for non-existent resource returns 404."
  (ecard-carddav-map-test--setup)
  (unwind-protect
      (should-error
       (ecard-carddav-get-resource
        ecard-carddav-map-test--addressbook
        "/addressbooks/user/contacts/nonexistent.vcf")
       :type 'ecard-carddav-not-found-error)
    (ecard-carddav-map-test--teardown)))

(ert-deftest ecard-carddav-map-test-mock-handle-put-new ()
  "Test mock PUT for new resource returns 201."
  (ecard-carddav-map-test--setup)
  (unwind-protect
      (let ((result (ecard-carddav-put-ecard
                    ecard-carddav-map-test--addressbook
                    "/addressbooks/user/contacts/new.vcf"
                    (ecard-carddav-map-test--create-test-ecard "New Contact"))))
        (should (ecard-carddav-resource-p result))
        (should (oref result etag)))
    (ecard-carddav-map-test--teardown)))

(ert-deftest ecard-carddav-map-test-mock-handle-put-update ()
  "Test mock PUT for existing resource returns 204."
  (ecard-carddav-map-test--setup)
  (unwind-protect
      (let ((path "/addressbooks/user/contacts/update.vcf"))
        ;; Create
        (let ((r1 (ecard-carddav-put-ecard
                   ecard-carddav-map-test--addressbook path
                   (ecard-carddav-map-test--create-test-ecard "Original"))))
          ;; Update with matching etag
          (let ((r2 (ecard-carddav-put-ecard
                     ecard-carddav-map-test--addressbook path
                     (ecard-carddav-map-test--create-test-ecard "Updated")
                     (oref r1 etag))))
            (should (ecard-carddav-resource-p r2))
            (should-not (string= (oref r1 etag) (oref r2 etag))))))
    (ecard-carddav-map-test--teardown)))

(ert-deftest ecard-carddav-map-test-mock-handle-put-etag-mismatch ()
  "Test mock PUT with wrong ETag returns 412."
  (ecard-carddav-map-test--setup)
  (unwind-protect
      (let ((path "/addressbooks/user/contacts/conflict.vcf"))
        (ecard-carddav-put-ecard
         ecard-carddav-map-test--addressbook path
         (ecard-carddav-map-test--create-test-ecard "Original"))
        ;; Try to update with wrong etag
        (should-error
         (ecard-carddav-put-ecard
          ecard-carddav-map-test--addressbook path
          (ecard-carddav-map-test--create-test-ecard "Conflict")
          "wrong-etag")
         :type 'ecard-carddav-conflict-error))
    (ecard-carddav-map-test--teardown)))

(ert-deftest ecard-carddav-map-test-mock-handle-delete-not-found ()
  "Test mock DELETE for non-existent resource returns 404."
  (ecard-carddav-map-test--setup)
  (unwind-protect
      (should-error
       (ecard-carddav-delete-resource
        ecard-carddav-map-test--addressbook
        "/addressbooks/user/contacts/ghost.vcf")
       :type 'ecard-carddav-not-found-error)
    (ecard-carddav-map-test--teardown)))

(ert-deftest ecard-carddav-map-test-mock-handle-delete-etag-mismatch ()
  "Test mock DELETE with wrong ETag returns 412."
  (ecard-carddav-map-test--setup)
  (unwind-protect
      (let ((path "/addressbooks/user/contacts/delete-conflict.vcf"))
        (ecard-carddav-put-ecard
         ecard-carddav-map-test--addressbook path
         (ecard-carddav-map-test--create-test-ecard "Victim"))
        (should-error
         (ecard-carddav-delete-resource
          ecard-carddav-map-test--addressbook path "wrong-etag")
         :type 'ecard-carddav-conflict-error))
    (ecard-carddav-map-test--teardown)))

;;; Mock REPORT handler tests

(ert-deftest ecard-carddav-map-test-mock-sync-collection ()
  "Test mock sync-collection REPORT handler."
  (ecard-carddav-map-test--setup)
  (unwind-protect
      (let ((mock ecard-carddav-map-test--mock-server)
            (path "/addressbooks/user/contacts/"))
        ;; Add resources
        (ecard-carddav-mock-put-ecard
         mock (concat path "a.vcf")
         (ecard-carddav-map-test--create-test-ecard "Alice"))
        (ecard-carddav-mock-put-ecard
         mock (concat path "b.vcf")
         (ecard-carddav-map-test--create-test-ecard "Bob"))

        ;; Build sync-collection request
        (let ((xml (ecard-carddav-mock--parse-request-body
                    (concat "<?xml version=\"1.0\"?>"
                           "<sync-collection xmlns=\"DAV:\">"
                           "<sync-token>0</sync-token>"
                           "<prop><getetag/></prop>"
                           "</sync-collection>"))))
          (let ((response (ecard-carddav-mock--handle-sync-collection
                          mock path xml)))
            (should (= (plist-get response :status) 207))
            (should (string-match-p "sync-token" (plist-get response :body))))))
    (ecard-carddav-map-test--teardown)))

(ert-deftest ecard-carddav-map-test-mock-query ()
  "Test mock addressbook-query REPORT handler."
  (ecard-carddav-map-test--setup)
  (unwind-protect
      (let ((mock ecard-carddav-map-test--mock-server)
            (path "/addressbooks/user/contacts/"))
        (ecard-carddav-mock-put-ecard
         mock (concat path "a.vcf")
         (ecard-carddav-map-test--create-test-ecard "Alice"))

        (let ((xml (ecard-carddav-mock--parse-request-body
                    (concat "<?xml version=\"1.0\"?>"
                           "<C:addressbook-query xmlns=\"DAV:\" xmlns:C=\"urn:ietf:params:xml:ns:carddav\">"
                           "<prop><getetag/><C:address-data/></prop>"
                           "</C:addressbook-query>"))))
          (let ((response (ecard-carddav-mock--handle-query mock path xml)))
            (should (= (plist-get response :status) 207))
            (should (string-match-p "address-data" (plist-get response :body))))))
    (ecard-carddav-map-test--teardown)))

(ert-deftest ecard-carddav-map-test-mock-report-unsupported ()
  "Test mock server rejects unsupported REPORT types."
  (ecard-carddav-map-test--setup)
  (unwind-protect
      (let ((mock ecard-carddav-map-test--mock-server))
        ;; Pass string body, not parsed XML - handle-report parses it internally
        (let ((response (ecard-carddav-mock--handle-report
                        mock "/addressbooks/user/contacts/"
                        "<unknown-report xmlns=\"DAV:\"/>")))
          (should (= (plist-get response :status) 400))))
    (ecard-carddav-map-test--teardown)))

;;; Mock PROPFIND handler tests

(ert-deftest ecard-carddav-map-test-mock-propfind-principal ()
  "Test mock principal discovery PROPFIND."
  (ecard-carddav-map-test--setup)
  (unwind-protect
      (let ((response (ecard-carddav-mock--propfind-principal
                      ecard-carddav-map-test--mock-server)))
        (should (= (plist-get response :status) 207))
        (should (string-match-p "current-user-principal"
                               (plist-get response :body))))
    (ecard-carddav-map-test--teardown)))

(ert-deftest ecard-carddav-map-test-mock-propfind-principal-props ()
  "Test mock principal properties PROPFIND."
  (ecard-carddav-map-test--setup)
  (unwind-protect
      (let ((response (ecard-carddav-mock--propfind-principal-props
                      ecard-carddav-map-test--mock-server)))
        (should (= (plist-get response :status) 207))
        (should (string-match-p "addressbook-home-set"
                               (plist-get response :body))))
    (ecard-carddav-map-test--teardown)))

(ert-deftest ecard-carddav-map-test-mock-propfind-addressbook-depth1 ()
  "Test mock addressbook PROPFIND with depth 1 includes resources."
  (ecard-carddav-map-test--setup)
  (unwind-protect
      (progn
        (ecard-carddav-mock-put-ecard
         ecard-carddav-map-test--mock-server
         "/addressbooks/user/contacts/test.vcf"
         (ecard-carddav-map-test--create-test-ecard "Test"))
        (let ((response (ecard-carddav-mock--propfind-addressbook
                        ecard-carddav-map-test--mock-server
                        "/addressbooks/user/contacts/"
                        "1")))
          (should (= (plist-get response :status) 207))
          (should (string-match-p "getetag"
                                 (plist-get response :body)))))
    (ecard-carddav-map-test--teardown)))

(ert-deftest ecard-carddav-map-test-mock-propfind-home-depth1-lists-addressbooks ()
  "Test mock addressbook-home PROPFIND with depth 1 lists addressbooks."
  (ecard-carddav-map-test--setup)
  (unwind-protect
      (let ((response (ecard-carddav-mock--propfind-addressbook-home
                      ecard-carddav-map-test--mock-server
                      "1")))
        (should (= (plist-get response :status) 207))
        (should (string-match-p "Test Contacts"
                               (plist-get response :body))))
    (ecard-carddav-map-test--teardown)))

(ert-deftest ecard-carddav-map-test-mock-propfind-home-depth1 ()
  "Test mock addressbook-home PROPFIND with depth 1 lists addressbooks."
  (ecard-carddav-map-test--setup)
  (unwind-protect
      (let ((response (ecard-carddav-mock--propfind-addressbook-home
                      ecard-carddav-map-test--mock-server
                      "1")))
        (should (= (plist-get response :status) 207))
        (should (string-match-p "Test Contacts"
                               (plist-get response :body))))
    (ecard-carddav-map-test--teardown)))

;;; Multiget fallback tests

(ert-deftest ecard-carddav-map-test-multiget-fallback-individual ()
  "Test fallback to individual fetches when multiget fails."
  (ecard-carddav-map-test--setup)
  (unwind-protect
      (progn
        (ecard-carddav-map-test--populate-addressbook 3)

        ;; Mock multiget to fail, forcing individual fetch fallback
        (let ((orig-fn (symbol-function 'ecard-carddav-multiget-resources)))
          (cl-letf (((symbol-function 'ecard-carddav-multiget-resources)
                     (lambda (_ab _paths)
                       (error "Multiget not supported"))))
            (let ((result (ecard-carddav-map-resources
                          ecard-carddav-map-test--addressbook
                          (lambda (resource)
                            (let ((ecard (oref resource ecard)))
                              (ecard-add-property ecard 'note "Fallback")
                              t)))))
              ;; Should still process all resources via individual fetches
              (should (= (plist-get result :total) 3))
              (should (= (plist-get result :processed) 3))
              (should (= (plist-get result :modified) 3))))))
    (ecard-carddav-map-test--teardown)))

(ert-deftest ecard-carddav-map-test-multiget-fallback-partial-failure ()
  "Test fallback when multiget fails and some individual fetches also fail."
  (ecard-carddav-map-test--setup)
  (unwind-protect
      (progn
        (ecard-carddav-map-test--populate-addressbook 3)

        ;; Mock multiget to fail and one individual fetch to fail
        (let ((fetch-count 0))
          (cl-letf (((symbol-function 'ecard-carddav-multiget-resources)
                     (lambda (_ab _paths)
                       (error "Multiget not supported")))
                    ((symbol-function 'ecard-carddav-get-resource)
                     (let ((orig-fn (symbol-function 'ecard-carddav-get-resource)))
                       (lambda (ab path)
                         (setq fetch-count (1+ fetch-count))
                         (if (= fetch-count 2)
                             (error "Simulated fetch error")
                           (funcall orig-fn ab path))))))
            (let ((result (ecard-carddav-map-resources
                          ecard-carddav-map-test--addressbook
                          (lambda (_resource) nil))))
              ;; Should have 1 fetch error
              (should (>= (plist-get result :failed) 1))
              ;; Should still process the ones that succeeded
              (should (>= (plist-get result :processed) 2))))))
    (ecard-carddav-map-test--teardown)))

;;; Serialization error in change detection

(ert-deftest ecard-carddav-map-test-serialize-error ()
  "Test serialization error during change detection."
  (ecard-carddav-map-test--setup)
  (unwind-protect
      (progn
        (ecard-carddav-map-test--populate-addressbook 2)

        (let ((serialize-count 0))
          (cl-letf (((symbol-function 'ecard-serialize)
                     (let ((orig-fn (symbol-function 'ecard-serialize)))
                       (lambda (ecard-obj)
                         (setq serialize-count (1+ serialize-count))
                         ;; Fail on second serialization (first resource's "before" snapshot)
                         ;; count 1 = first resource serialization
                         (if (= serialize-count 1)
                             (error "Simulated serialize error")
                           (funcall orig-fn ecard-obj))))))
            (let ((result (ecard-carddav-map-resources
                          ecard-carddav-map-test--addressbook
                          (lambda (resource)
                            (let ((ecard (oref resource ecard)))
                              (ecard-add-property ecard 'note "Test")
                              t)))))
              ;; At least one failure from serialization error
              (should (>= (plist-get result :failed) 1))
              ;; Should have a serialize-error type in errors
              (should (cl-some (lambda (err)
                                (eq (plist-get err :type) 'serialize-error))
                              (plist-get result :errors)))))))
    (ecard-carddav-map-test--teardown)))

;;; Transformation error - additional coverage

(ert-deftest ecard-carddav-map-test-transformation-all-errors ()
  "Test all resources erroring in transformation."
  (ecard-carddav-map-test--setup)
  (unwind-protect
      (progn
        (ecard-carddav-map-test--populate-addressbook 3)
        (let ((result (ecard-carddav-map-resources
                      ecard-carddav-map-test--addressbook
                      (lambda (_resource)
                        (error "Transform failed")))))
          (should (= (plist-get result :total) 3))
          (should (= (plist-get result :processed) 3))
          (should (= (plist-get result :failed) 3))
          (should (= (length (plist-get result :errors)) 3))
          ;; All should be transform-error
          (dolist (err (plist-get result :errors))
            (should (eq (plist-get err :type) 'transform-error)))))
    (ecard-carddav-map-test--teardown)))

;;; Update error paths (non-conflict errors)

(ert-deftest ecard-carddav-map-test-update-generic-error ()
  "Test generic error during resource update."
  (ecard-carddav-map-test--setup)
  (unwind-protect
      (progn
        (ecard-carddav-map-test--populate-addressbook 1)

        ;; Mock put-ecard to signal a generic error (not conflict)
        (cl-letf (((symbol-function 'ecard-carddav-put-ecard)
                   (lambda (_ab _path _ecard &optional _etag _le)
                     (error "Disk full"))))
          (let ((result (ecard-carddav-map-resources
                        ecard-carddav-map-test--addressbook
                        (lambda (resource)
                          (let ((ecard (oref resource ecard)))
                            (ecard-add-property ecard 'note "Test")
                            t)))))
            (should (= (plist-get result :failed) 1))
            ;; Should have update-error type
            (should (cl-some (lambda (err)
                              (eq (plist-get err :type) 'update-error))
                            (plist-get result :errors))))))
    (ecard-carddav-map-test--teardown)))

;;; Retry-once failure path

(ert-deftest ecard-carddav-map-test-handle-resource-update-retry-fails ()
  "Test retry-once conflict when retry also fails."
  (ecard-carddav-map-test--setup)
  (unwind-protect
      (progn
        (ecard-carddav-map-test--populate-addressbook 1)
        (let* ((resources (ecard-carddav-list-resources
                          ecard-carddav-map-test--addressbook))
               (resource-info (car resources))
               (resource (ecard-carddav-get-resource
                         ecard-carddav-map-test--addressbook
                         (oref resource-info path)))
               (stats (ecard-carddav-map--make-stats)))
          ;; Invalidate etag and mock get-resource to fail on retry
          (oset resource etag "invalid-etag")
          (ecard-add-property (oref resource ecard) 'note "Retry fail")
          (cl-letf (((symbol-function 'ecard-carddav-get-resource)
                     (lambda (_ab _path)
                       (error "Cannot re-fetch resource"))))
            (let ((result (ecard-carddav-map--handle-resource-update
                          ecard-carddav-map-test--addressbook
                          resource stats :retry-once)))
              (should (= (plist-get result :failed) 1))
              ;; Should be retry-failed type
              (should (cl-some (lambda (err)
                                (eq (plist-get err :type) 'retry-failed))
                              (plist-get result :errors)))))))
    (ecard-carddav-map-test--teardown)))

;;; Force failure path for update

(ert-deftest ecard-carddav-map-test-handle-resource-update-force-fails ()
  "Test force update when force also fails."
  (ecard-carddav-map-test--setup)
  (unwind-protect
      (progn
        (ecard-carddav-map-test--populate-addressbook 1)
        (let* ((resources (ecard-carddav-list-resources
                          ecard-carddav-map-test--addressbook))
               (resource-info (car resources))
               (resource (ecard-carddav-get-resource
                         ecard-carddav-map-test--addressbook
                         (oref resource-info path)))
               (stats (ecard-carddav-map--make-stats)))
          ;; Invalidate etag and mock get-resource to fail
          (oset resource etag "invalid-etag")
          (ecard-add-property (oref resource ecard) 'note "Force fail")
          (cl-letf (((symbol-function 'ecard-carddav-get-resource)
                     (lambda (_ab _path)
                       (error "Cannot re-fetch for force"))))
            (let ((result (ecard-carddav-map--handle-resource-update
                          ecard-carddav-map-test--addressbook
                          resource stats :force)))
              (should (= (plist-get result :failed) 1))
              ;; Should be force-failed type
              (should (cl-some (lambda (err)
                                (eq (plist-get err :type) 'force-failed))
                              (plist-get result :errors)))))))
    (ecard-carddav-map-test--teardown)))

;;; Delete force failure path

(ert-deftest ecard-carddav-map-test-handle-resource-delete-force-fails ()
  "Test force delete when force also fails."
  (ecard-carddav-map-test--setup)
  (unwind-protect
      (progn
        (ecard-carddav-map-test--populate-addressbook 1)
        (let* ((resources (ecard-carddav-list-resources
                          ecard-carddav-map-test--addressbook))
               (resource-info (car resources))
               (resource (ecard-carddav-get-resource
                         ecard-carddav-map-test--addressbook
                         (oref resource-info path)))
               (stats (ecard-carddav-map--make-stats)))
          ;; Invalidate etag and mock get-resource to fail
          (oset resource etag "invalid-etag")
          (cl-letf (((symbol-function 'ecard-carddav-get-resource)
                     (lambda (_ab _path)
                       (error "Cannot re-fetch for force delete"))))
            (let ((result (ecard-carddav-map--handle-resource-delete
                          ecard-carddav-map-test--addressbook
                          resource stats :force)))
              (should (= (plist-get result :failed) 1))
              ;; Should be delete-force-failed type
              (should (cl-some (lambda (err)
                                (eq (plist-get err :type) 'delete-force-failed))
                              (plist-get result :errors)))))))
    (ecard-carddav-map-test--teardown)))

;;; Delete generic error path

(ert-deftest ecard-carddav-map-test-handle-resource-delete-generic-error ()
  "Test generic error during resource deletion."
  (ecard-carddav-map-test--setup)
  (unwind-protect
      (progn
        (ecard-carddav-map-test--populate-addressbook 1)
        (let* ((resources (ecard-carddav-list-resources
                          ecard-carddav-map-test--addressbook))
               (resource-info (car resources))
               (resource (ecard-carddav-get-resource
                         ecard-carddav-map-test--addressbook
                         (oref resource-info path)))
               (stats (ecard-carddav-map--make-stats)))
          ;; Mock delete-resource to signal a generic error
          (cl-letf (((symbol-function 'ecard-carddav-delete-resource)
                     (lambda (_ab _path &optional _etag)
                       (error "Network timeout"))))
            (let ((result (ecard-carddav-map--handle-resource-delete
                          ecard-carddav-map-test--addressbook
                          resource stats :skip)))
              (should (= (plist-get result :failed) 1))
              ;; Should be delete-error type
              (should (cl-some (lambda (err)
                                (eq (plist-get err :type) 'delete-error))
                              (plist-get result :errors)))))))
    (ecard-carddav-map-test--teardown)))

;;; End-to-end map with all error paths exercised

(ert-deftest ecard-carddav-map-test-map-with-delete-errors ()
  "Test mapping with delete operations that encounter errors."
  (ecard-carddav-map-test--setup)
  (unwind-protect
      (progn
        (ecard-carddav-map-test--populate-addressbook 3)

        ;; Mock delete-resource to fail generically
        (let ((orig-fn (symbol-function 'ecard-carddav-delete-resource)))
          (cl-letf (((symbol-function 'ecard-carddav-delete-resource)
                     (lambda (ab path &optional etag)
                       (if (string-match "contact1" path)
                           (error "Permission denied")
                         (funcall orig-fn ab path etag)))))
            (let ((result (ecard-carddav-map-resources
                          ecard-carddav-map-test--addressbook
                          (lambda (_resource) :delete))))
              ;; contact1 should fail, contacts 2 and 3 should succeed
              (should (= (plist-get result :deleted) 2))
              (should (= (plist-get result :failed) 1))))))
    (ecard-carddav-map-test--teardown)))

(provide 'ecard-carddav-map-test)
;;; ecard-carddav-map-test.el ends here
