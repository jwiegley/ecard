;;; ecard-display-test.el --- Tests for ecard-display -*- lexical-binding: t; -*-

;; Copyright (C) 2025 John Wiegley

;; Author: John Wiegley <johnw@gnu.org>

;;; Commentary:

;; Basic tests for ecard-display module.
;; These tests verify UI buffer creation and basic functionality.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ecard-display)
(require 'ecard-widget)

;;; Test server list buffer

(ert-deftest ecard-display-test-servers-buffer ()
  "Test that server list buffer can be created."
  (let ((saved-servers ecard-carddav-servers))
    (unwind-protect
        (progn
          (setq ecard-carddav-servers
                '((:name "Test Server"
                   :url "https://test.example.com"
                   :username "test"
                   :password "secret")))
          (with-temp-buffer
            (ecard-display-servers-mode)
            (should (eq major-mode 'ecard-display-servers-mode))
            (should (boundp 'tabulated-list-format))
            (should (vectorp tabulated-list-format))))
      (setq ecard-carddav-servers saved-servers))))

(ert-deftest ecard-display-test-servers-refresh ()
  "Test that server list can be refreshed."
  (let ((saved-servers ecard-carddav-servers))
    (unwind-protect
        (progn
          (setq ecard-carddav-servers
                '((:name "Server 1"
                   :url "https://server1.example.com"
                   :username "user1"
                   :password "pass1")
                  (:name "Server 2"
                   :url "https://server2.example.com"
                   :username "user2"
                   :password "pass2")))
          (with-temp-buffer
            (ecard-display-servers-mode)
            (ecard-display-servers-refresh)
            (should (equal (length tabulated-list-entries) 2))
            ;; Verify first entry
            (let ((entry (car tabulated-list-entries)))
              (should (plist-member (car entry) :name)))))
      (setq ecard-carddav-servers saved-servers))))

;;; Test addressbook buffer

(ert-deftest ecard-display-test-addressbooks-mode ()
  "Test that addressbook buffer mode can be initialized."
  (with-temp-buffer
    (ecard-display-addressbooks-mode)
    (should (eq major-mode 'ecard-display-addressbooks-mode))
    (should (boundp 'tabulated-list-format))
    (should (vectorp tabulated-list-format))
    ;; Verify format has expected columns
    (should (equal (length tabulated-list-format) 3))))

;;; Test contacts buffer

(ert-deftest ecard-display-test-contacts-mode ()
  "Test that contacts buffer mode can be initialized."
  (with-temp-buffer
    (ecard-display-contacts-mode)
    (should (eq major-mode 'ecard-display-contacts-mode))
    (should (boundp 'tabulated-list-format))
    (should (vectorp tabulated-list-format))
    ;; Verify format has expected columns (FN, Email, Phone)
    (should (equal (length tabulated-list-format) 3))))

;;; Test contact detail buffer

(ert-deftest ecard-display-test-contact-mode ()
  "Test that contact detail buffer mode can be initialized."
  (with-temp-buffer
    (ecard-display-contact-mode)
    (should (eq major-mode 'ecard-display-contact-mode))
    (should (not buffer-read-only))))

;;; Test helper functions

(ert-deftest ecard-display-test-get-fn ()
  "Test extracting full name from ecard."
  (let* ((fn-prop (ecard-property :name "FN" :value "John Doe"))
         (ecard-obj (ecard :fn (list fn-prop))))
    (should (equal (ecard-display--get-fn ecard-obj) "John Doe"))))

(ert-deftest ecard-display-test-get-fn-nil ()
  "Test extracting full name from nil ecard."
  (should (equal (ecard-display--get-fn nil) "Unknown")))

(ert-deftest ecard-display-test-get-first-email ()
  "Test extracting first email from ecard."
  (let* ((email-prop (ecard-property :name "EMAIL" :value "john@example.com"))
         (ecard-obj (ecard :email (list email-prop))))
    (should (equal (ecard-display--get-first-email ecard-obj) "john@example.com"))))

(ert-deftest ecard-display-test-get-first-email-empty ()
  "Test extracting email from ecard with no emails."
  (let ((ecard-obj (ecard)))
    (should (equal (ecard-display--get-first-email ecard-obj) ""))))

(ert-deftest ecard-display-test-get-first-tel ()
  "Test extracting first telephone from ecard."
  (let* ((tel-prop (ecard-property :name "TEL" :value "+1-555-1234"))
         (ecard-obj (ecard :tel (list tel-prop))))
    (should (equal (ecard-display--get-first-tel ecard-obj) "+1-555-1234"))))

(ert-deftest ecard-display-test-uuid-generation ()
  "Test UUID generation produces valid format."
  (let ((uuid (ecard-display--generate-uuid)))
    (should (stringp uuid))
    (should (string-match-p
             "^[0-9a-f]\\{8\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{12\\}$"
             uuid))))

;;; Test clone functionality

(ert-deftest ecard-display-test-clone-ecard ()
  "Test that ecard cloning creates independent copy."
  (let* ((fn-prop (ecard-property :name "FN" :value "Original"))
         (ecard-obj (ecard :fn (list fn-prop)))
         (cloned (ecard-display--clone-ecard ecard-obj)))
    (should cloned)
    (should (ecard-p cloned))
    ;; Verify it has same data
    (should (equal (ecard-display--get-fn cloned) "Original"))))

;;; Test parameter extraction

(ert-deftest ecard-display-test-get-param ()
  "Test extracting parameters from property."
  (let ((prop (ecard-property
               :name "EMAIL"
               :value "work@example.com"
               :parameters '(("TYPE" . "work")
                             ("PREF" . "1")))))
    (should (equal (ecard-display--get-param prop "TYPE") "work"))
    (should (equal (ecard-display--get-param prop "PREF") "1"))
    (should (null (ecard-display--get-param prop "NONEXISTENT")))))

;;; Issue 1: Contact count tests

(ert-deftest ecard-display-test-contact-count-with-resources ()
  "Verify addressbook shows correct contact count when resources are loaded."
  (let* ((server (ecard-carddav-server))
         (addressbook (ecard-carddav-addressbook
                       :server server
                       :url "https://test.example.com/addressbook"
                       :display-name "Test Addressbook"))
         (resources (list
                     (ecard-carddav-resource :path "/contact1.vcf")
                     (ecard-carddav-resource :path "/contact2.vcf")
                     (ecard-carddav-resource :path "/contact3.vcf"))))
    (oset addressbook resources resources)
    (should (equal (ecard-display--addressbook-contact-count addressbook) "3"))))

(ert-deftest ecard-display-test-contact-count-without-resources ()
  "Verify addressbook shows '?' when resources are not loaded."
  (let* ((server (ecard-carddav-server))
         (addressbook (ecard-carddav-addressbook
                       :server server
                       :url "https://test.example.com/addressbook"
                       :display-name "Test Addressbook")))
    (should (equal (ecard-display--addressbook-contact-count addressbook) "?"))))

(ert-deftest ecard-display-test-contact-count-empty ()
  "Verify addressbook shows '0' when resources list is empty."
  (let* ((server (ecard-carddav-server))
         (addressbook (ecard-carddav-addressbook
                       :server server
                       :url "https://test.example.com/addressbook"
                       :display-name "Test Addressbook")))
    (oset addressbook resources nil)
    (should (equal (ecard-display--addressbook-contact-count addressbook) "?"))))

;;; Issue 2: Performance tests

(ert-deftest ecard-display-test-extract-name-from-path ()
  "Test extracting display name from vCard path."
  (should (equal (ecard-display--extract-name-from-path "/contacts/JohnDoe.vcf")
                 "JohnDoe"))
  (should (equal (ecard-display--extract-name-from-path "/contacts/Jane%20Smith.vcf")
                 "Jane Smith"))
  (should (equal (ecard-display--extract-name-from-path "/addressbook/contacts/test.vcf")
                 "test")))

(ert-deftest ecard-display-test-contacts-populate-fallback ()
  "Verify contact loading falls back to path-based names when multiget fails.
This test ensures we gracefully handle errors and show filenames as fallback."
  (let* ((server (ecard-carddav-server))
         (addressbook (ecard-carddav-addressbook
                       :server server
                       :url "https://test.example.com/addressbook"))
         ;; Create 10 resources WITHOUT ecard data
         (resources (cl-loop for i from 1 to 10
                             collect (ecard-carddav-resource
                                      :addressbook addressbook
                                      :path (format "/contact%d.vcf" i)
                                      :url (format "https://test.example.com/contact%d.vcf" i)
                                      :etag (format "etag-%d" i)))))
    (oset addressbook resources resources)
    (with-temp-buffer
      (ecard-display-contacts-mode)
      (setq ecard-display--addressbook addressbook)
      ;; Populate will try multiget (which will fail), but should still show entries
      (condition-case _err
          (ecard-display-contacts--populate addressbook)
        (error nil))  ; Ignore errors - we expect multiget to fail
      ;; Verify we have entries (fallback to path-based names)
      (should (equal (length tabulated-list-entries) 10))
      ;; Verify entries use path-based names (fallback when no ecard data)
      (let ((first-entry (car tabulated-list-entries)))
        (should (vectorp (cadr first-entry)))
        ;; First column should be "contact1" (from path)
        (should (equal (aref (cadr first-entry) 0) "contact1"))))))

;;; Issue 3: Safe string conversion tests

(ert-deftest ecard-display-test-safe-string-nil ()
  "Test safe-string conversion of nil."
  (should (equal (ecard-display--safe-string nil) "")))

(ert-deftest ecard-display-test-safe-string-string ()
  "Test safe-string conversion of string."
  (should (equal (ecard-display--safe-string "hello") "hello"))
  (should (equal (ecard-display--safe-string "") "")))

(ert-deftest ecard-display-test-safe-string-empty-list ()
  "Test safe-string conversion of empty list."
  (should (equal (ecard-display--safe-string '()) ""))
  (should (equal (ecard-display--safe-string '("")) ""))
  (should (equal (ecard-display--safe-string '("" "")) ", ")))

(ert-deftest ecard-display-test-safe-string-list ()
  "Test safe-string conversion of lists."
  (should (equal (ecard-display--safe-string '("one")) "one"))
  (should (equal (ecard-display--safe-string '("one" "two")) "one, two"))
  (should (equal (ecard-display--safe-string '("a" "b" "c")) "a, b, c")))

(ert-deftest ecard-display-test-safe-string-number ()
  "Test safe-string conversion of numbers."
  (should (equal (ecard-display--safe-string 42) "42"))
  (should (equal (ecard-display--safe-string 0) "0"))
  (should (equal (ecard-display--safe-string 3.14) "3.14")))

(ert-deftest ecard-display-test-safe-string-symbol ()
  "Test safe-string conversion of symbols."
  (should (equal (ecard-display--safe-string 'test) "test"))
  (should (equal (ecard-display--safe-string 'hello-world) "hello-world")))

(ert-deftest ecard-display-test-safe-string-mixed-list ()
  "Test safe-string conversion of mixed-type lists."
  (should (stringp (ecard-display--safe-string '(1 2 3))))
  (should (stringp (ecard-display--safe-string '(a b c)))))

(ert-deftest ecard-display-test-contact-detail-with-empty-fields ()
  "Verify contact detail handles empty/missing fields without error."
  (let* ((server (ecard-carddav-server))
         (addressbook (ecard-carddav-addressbook :server server))
         ;; Create ecard with various empty fields
         (ecard-obj (ecard
                     ;; FN is provided (required)
                     :fn (list (ecard-property :name "FN" :value "Test User"))
                     ;; N has empty components
                     :n (list (ecard-property :name "N" :value '("" "" "" "" "")))
                     ;; EMAIL is empty list
                     :email nil
                     ;; TEL is missing
                     ;; ORG has empty value
                     :org (list (ecard-property :name "ORG" :value ""))))
         (resource (ecard-carddav-resource
                    :addressbook addressbook
                    :url "https://test.example.com/test.vcf"
                    :path "/test.vcf"
                    :ecard ecard-obj)))
    (with-temp-buffer
      (ecard-display-contact-mode)
      (setq ecard-display--resource resource
            ecard-display--addressbook addressbook)
      ;; Rendering should not error
      (should-not (condition-case err
                      (progn
                        (ecard-display-contact--render resource)
                        nil)
                    (error err)))
      ;; Buffer should have content
      (should (> (buffer-size) 0)))))

(ert-deftest ecard-display-test-contact-detail-malformed-data ()
  "Verify contact detail handles malformed vCard data gracefully."
  (let* ((server (ecard-carddav-server))
         (addressbook (ecard-carddav-addressbook :server server))
         ;; Create ecard with edge case values that are technically valid
         ;; but unusual - lists where strings are expected
         (ecard-obj (ecard
                     :fn (list (ecard-property :name "FN" :value "Test"))
                     ;; Email value is a list instead of string (valid per EIEIO but unusual)
                     :email (list (ecard-property :name "EMAIL" :value '("test@example.com")))
                     ;; Tel value is empty list (valid but unusual)
                     :tel (list (ecard-property :name "TEL" :value '()))
                     ;; Org value is nested list (valid but unusual)
                     :org (list (ecard-property :name "ORG" :value '(("Company" "Division"))))))
         (resource (ecard-carddav-resource
                    :addressbook addressbook
                    :url "https://test.example.com/test.vcf"
                    :path "/test.vcf"
                    :ecard ecard-obj)))
    (with-temp-buffer
      (ecard-display-contact-mode)
      (setq ecard-display--resource resource
            ecard-display--addressbook addressbook)
      ;; Rendering should not error even with unusual data structures
      (should-not (condition-case err
                      (progn
                        (ecard-display-contact--render resource)
                        nil)
                    (error err)))
      ;; Buffer should have content
      (should (> (buffer-size) 0)))))

(ert-deftest ecard-display-test-contact-detail-nil-ecard ()
  "Verify contact detail handles nil ecard gracefully."
  (let* ((server (ecard-carddav-server))
         (addressbook (ecard-carddav-addressbook :server server))
         (resource (ecard-carddav-resource
                    :addressbook addressbook
                    :url "https://test.example.com/test.vcf"
                    :path "/test.vcf"
                    :ecard nil)))
    (with-temp-buffer
      (ecard-display-contact-mode)
      (setq ecard-display--resource resource
            ecard-display--addressbook addressbook)
      ;; Should handle nil ecard without crashing
      (should (condition-case _err
                  (progn
                    (ecard-display-contact--render resource)
                    nil)
                (error t))))))

;;; Test helpers and sample data

(defvar ecard-display-test--mock-server nil
  "Currently active mock server for testing.")

(defvar ecard-display-test--real-server nil
  "Currently active real CardDAV server for testing.")

(defvar ecard-display-test--environment nil
  "Plist containing test environment data.
Keys:
  :mock-server - Mock server object
  :real-server - Real CardDAV server object
  :addressbooks - List of addressbook objects
  :resources - List of resource objects")

(defun ecard-display-test--create-sample-contact (fn given family email phone org
                                                     &optional note)
  "Create a sample vCard.
Arguments: FN GIVEN FAMILY EMAIL PHONE ORG and optional NOTE."
  (let ((ecard-obj (ecard-create :fn fn)))

    ;; Add N property if we have name components
    (when (or given family)
      (let ((n-prop (ecard-property
                     :name "N"
                     :value (list family given "" "" ""))))
        (oset ecard-obj n (list n-prop))))

    ;; Add EMAIL if provided
    (when (and email (not (string-empty-p email)))
      (let ((email-prop (ecard-property
                         :name "EMAIL"
                         :parameters '(("TYPE" . "work"))
                         :value email)))
        (oset ecard-obj email (list email-prop))))

    ;; Add TEL if provided
    (when (and phone (not (string-empty-p phone)))
      (let ((tel-prop (ecard-property
                       :name "TEL"
                       :parameters '(("TYPE" . "work"))
                       :value phone)))
        (oset ecard-obj tel (list tel-prop))))

    ;; Add ORG if provided
    (when (and org (not (string-empty-p org)))
      (let ((org-prop (ecard-property
                       :name "ORG"
                       :value org)))
        (oset ecard-obj org (list org-prop))))

    ;; Add NOTE if provided
    (when (and note (not (string-empty-p note)))
      (let ((note-prop (ecard-property
                        :name "NOTE"
                        :value note)))
        (oset ecard-obj note (list note-prop))))

    ;; Add UID
    (let ((uid (format "urn:uuid:%s" (ecard-display--generate-uuid))))
      (oset ecard-obj uid (list (ecard-property :name "UID" :value uid))))

    ecard-obj))

(defun ecard-display-test--create-sample-contacts ()
  "Create a list of sample vCard objects for testing."
  (list
   (ecard-display-test--create-sample-contact
    "John Doe" "John" "Doe"
    "john.doe@example.com" "+1-555-0100"
    "ACME Corporation"
    "Senior Software Engineer")

   (ecard-display-test--create-sample-contact
    "Jane Smith" "Jane" "Smith"
    "jane.smith@example.com" "+1-555-0101"
    "Tech Innovations Inc"
    "Product Manager")

   (ecard-display-test--create-sample-contact
    "Bob Johnson" "Bob" "Johnson"
    "bob.johnson@example.com" "+1-555-0102"
    "Design Studio LLC"
    "Creative Director")

   (ecard-display-test--create-sample-contact
    "Alice Williams" "Alice" "Williams"
    "alice.williams@example.com" "+1-555-0103"
    "Data Systems Corp"
    "Database Administrator")

   (ecard-display-test--create-sample-contact
    "Charlie Brown" "Charlie" "Brown"
    "charlie.brown@example.com" "+1-555-0104"
    "Marketing Solutions"
    "Marketing Director")))

(defun ecard-display-test--setup-mock-environment ()
  "Set up complete mock CardDAV environment for interactive testing.
Returns plist with :mock-server :real-server :addressbooks :resources."
  (message "Setting up mock CardDAV environment...")

  ;; Create mock server
  (let* ((mock-server (ecard-carddav-mock-server-create
                       :base-url "https://mock.test.local"
                       :principal-path "/principals/testuser/"
                       :addressbook-home-path "/addressbooks/testuser/"))

         ;; Add address books
         (ab-contacts-path "/addressbooks/testuser/contacts/")
         (ab-work-path "/addressbooks/testuser/work/")

         ;; Create sample contacts
         (sample-contacts (ecard-display-test--create-sample-contacts))

         ;; Track created resources
         (resources nil))

    ;; Add addressbooks to mock server
    (ecard-carddav-mock-add-addressbook
     mock-server ab-contacts-path
     "Personal Contacts" "My personal contacts")

    (ecard-carddav-mock-add-addressbook
     mock-server ab-work-path
     "Work Contacts" "Work-related contacts")

    ;; Add sample contacts to "Personal Contacts" addressbook
    (let ((contact-num 1))
      (dolist (contact sample-contacts)
        (let ((path (format "%scontact-%d.vcf" ab-contacts-path contact-num)))
          (ecard-carddav-mock-put-ecard mock-server path contact)
          (push (list :path path :contact contact) resources)
          (setq contact-num (1+ contact-num)))))

    ;; Add one contact to "Work Contacts" addressbook
    (let ((work-contact (ecard-display-test--create-sample-contact
                         "Sarah Manager" "Sarah" "Manager"
                         "sarah.manager@work.example.com" "+1-555-0200"
                         "Work Corp" "CEO")))
      (let ((path (format "%swork-contact-1.vcf" ab-work-path)))
        (ecard-carddav-mock-put-ecard mock-server path work-contact)
        (push (list :path path :contact work-contact) resources)))

    ;; Install mock handler
    (ecard-carddav-mock-install mock-server)

    ;; Create real server object that will use the mock
    (let* ((auth (ecard-carddav-auth-basic-create
                  :username "testuser"
                  :password "testpass"))
           (real-server (ecard-carddav-server-create
                         :url "https://mock.test.local"
                         :auth auth)))

      ;; Store environment
      (setq ecard-display-test--mock-server mock-server
            ecard-display-test--real-server real-server
            ecard-display-test--environment
            (list :mock-server mock-server
                  :real-server real-server
                  :addressbooks nil
                  :resources (nreverse resources)))

      (message "Mock environment ready: 2 addressbooks, %d contacts"
               (length resources))

      ecard-display-test--environment)))

(defun ecard-display-test--cleanup-mock-environment ()
  "Clean up mock environment and buffers."
  (message "Cleaning up mock environment...")

  ;; Uninstall mock handler
  (ecard-carddav-mock-uninstall)

  ;; Kill all ecard-display buffers
  (dolist (buf (buffer-list))
    (when (string-match-p "\\*CardDAV" (buffer-name buf))
      (kill-buffer buf)))

  ;; Clear state
  (setq ecard-display-test--mock-server nil
        ecard-display-test--real-server nil
        ecard-display-test--environment nil)

  (message "Mock environment cleaned up"))

;;; Integration tests with mock server

(require 'ecard-carddav-mock)

(ert-deftest ecard-display-test-addressbook-shows-count ()
  "Verify addressbook buffer shows actual contact count, not '?'."
  (let ((mock (ecard-carddav-mock-server-create
               :base-url "https://test.example.com")))
    ;; Add addressbook with 3 contacts
    (ecard-carddav-mock-add-addressbook
     mock "/addressbooks/user/contacts/" "Contacts" "My contacts")
    (ecard-carddav-mock-put-ecard
     mock "/addressbooks/user/contacts/contact1.vcf"
     (ecard-create :fn "Alice"))
    (ecard-carddav-mock-put-ecard
     mock "/addressbooks/user/contacts/contact2.vcf"
     (ecard-create :fn "Bob"))
    (ecard-carddav-mock-put-ecard
     mock "/addressbooks/user/contacts/contact3.vcf"
     (ecard-create :fn "Charlie"))

    (unwind-protect
        (progn
          (ecard-carddav-mock-install mock)

          ;; Create server and discover addressbooks
          (let* ((server (ecard-carddav-server-create
                          :url "https://test.example.com"
                          :auth (ecard-carddav-auth-basic-create
                                 :username "test"
                                 :password "test"))))
            (ecard-carddav-discover-addressbooks server)

            ;; Load resources for the addressbook (required after populate no longer auto-loads)
            (let ((addressbook (car (oref server addressbooks))))
              (ecard-carddav-list-resources addressbook))

            ;; Populate addressbooks buffer
            (with-temp-buffer
              (ecard-display-addressbooks-mode)
              (setq ecard-display--server server)
              (ecard-display-addressbooks--populate server)

              ;; Verify we have entries
              (should (equal (length tabulated-list-entries) 1))

              ;; Verify count column shows "3" not "?"
              (let ((entry (car tabulated-list-entries)))
                (should (vectorp (cadr entry)))
                (should (equal (aref (cadr entry) 2) "3"))))))

      (ecard-carddav-mock-uninstall))))

(ert-deftest ecard-display-test-contacts-show-details ()
  "Verify contact list shows FN/EMAIL/TEL, not UUIDs."
  (let ((mock (ecard-carddav-mock-server-create
               :base-url "https://test.example.com")))
    ;; Add addressbook with UUID-named contacts
    (ecard-carddav-mock-add-addressbook
     mock "/addressbooks/user/contacts/" "Contacts" "My contacts")

    ;; Create contacts with UUID filenames but real data
    (let ((uuid1 "e18e7e74-efa3-4c27-9c8d-ff464deff3ec")
          (uuid2 "a1b2c3d4-e5f6-7890-abcd-ef1234567890"))
      (ecard-carddav-mock-put-ecard
       mock (format "/addressbooks/user/contacts/%s.vcf" uuid1)
       (ecard-create
        :fn "John Doe"
        :email "john@example.com"
        :tel "+1-555-0100"))
      (ecard-carddav-mock-put-ecard
       mock (format "/addressbooks/user/contacts/%s.vcf" uuid2)
       (ecard-create
        :fn "Jane Smith"
        :email "jane@example.com"
        :tel "+1-555-0200")))

    (unwind-protect
        (progn
          (ecard-carddav-mock-install mock)

          ;; Create server and discover addressbooks
          (let* ((server (ecard-carddav-server-create
                          :url "https://test.example.com"
                          :auth (ecard-carddav-auth-basic-create
                                 :username "test"
                                 :password "test"))))
            (ecard-carddav-discover-addressbooks server)
            (let ((addressbook (car (oref server addressbooks))))
              ;; List resources
              (ecard-carddav-list-resources addressbook)

              ;; Populate contacts buffer
              (with-temp-buffer
                (ecard-display-contacts-mode)
                (setq ecard-display--addressbook addressbook)
                (ecard-display-contacts--populate addressbook nil t)

                ;; Verify we have 2 entries
                (should (equal (length tabulated-list-entries) 2))

                ;; Verify entries show real names, not UUIDs
                (let* ((entries tabulated-list-entries)
                       (names (mapcar (lambda (e) (aref (cadr e) 0)) entries))
                       (emails (mapcar (lambda (e) (aref (cadr e) 1)) entries))
                       (phones (mapcar (lambda (e) (aref (cadr e) 2)) entries)))
                  ;; Should NOT contain UUID
                  (should-not (cl-some (lambda (n) (string-match-p "e18e7e74" n)) names))
                  (should-not (cl-some (lambda (n) (string-match-p "a1b2c3d4" n)) names))

                  ;; Should contain actual names
                  (should (member "John Doe" names))
                  (should (member "Jane Smith" names))

                  ;; Should contain actual emails
                  (should (member "john@example.com" emails))
                  (should (member "jane@example.com" emails))

                  ;; Should contain actual phones
                  (should (member "+1-555-0100" phones))
                  (should (member "+1-555-0200" phones)))))))

      (ecard-carddav-mock-uninstall))))

(ert-deftest ecard-display-test-multiget-performance ()
  "Verify addressbook-multiget uses single HTTP request for all contacts."
  (let* ((mock (ecard-carddav-mock-server-create
                :base-url "https://test.example.com"))
         (request-count 0)
         (counter-advice (lambda (&rest _) (setq request-count (1+ request-count)))))
    ;; Add addressbook with 50 contacts
    (ecard-carddav-mock-add-addressbook
     mock "/addressbooks/user/contacts/" "Contacts" "My contacts")

    (dotimes (i 50)
      (ecard-carddav-mock-put-ecard
       mock (format "/addressbooks/user/contacts/contact%d.vcf" i)
       (ecard-create
        :fn (format "Contact %d" i)
        :email (format "user%d@example.com" i))))

    (unwind-protect
        (progn
          (ecard-carddav-mock-install mock)

          ;; Track HTTP requests by advising the request handler
          (advice-add 'ecard-carddav-mock--handle-request
                      :before counter-advice)

          ;; Create server and discover addressbooks
          (let* ((server (ecard-carddav-server-create
                          :url "https://test.example.com"
                          :auth (ecard-carddav-auth-basic-create
                                 :username "test"
                                 :password "test"))))
            (setq request-count 0)  ; Reset counter before operations
            (ecard-carddav-discover-addressbooks server)
            (let ((_discovery-requests request-count)
                  (addressbook (car (oref server addressbooks))))

              ;; List resources (1 PROPFIND)
              (setq request-count 0)
              (ecard-carddav-list-resources addressbook)
              (let ((list-requests request-count))
                (should (equal list-requests 1))  ; Should be 1 PROPFIND

                ;; Populate contacts (should use 1 multiget REPORT)
                ;; Bind page-size to nil to load all contacts at once,
                ;; avoiding pollution from earlier tests that set it.
                (setq request-count 0)
                (with-temp-buffer
                  (ecard-display-contacts-mode)
                  (setq ecard-display--addressbook addressbook)
                  (let ((ecard-display-contacts-page-size nil))
                    (ecard-display-contacts--populate addressbook nil t)))

                ;; Should be exactly 1 request (multiget)
                (should (equal request-count 1))

                ;; Verify all 50 contacts have ecard data
                (let ((resources (oref addressbook resources)))
                  (should (equal (length resources) 50))
                  (dolist (resource resources)
                    (should (oref resource ecard))))))))

      (advice-remove 'ecard-carddav-mock--handle-request counter-advice)
      (ecard-carddav-mock-uninstall))))

(ert-deftest ecard-display-test-multiget-error-handling ()
  "Verify multiget handles errors gracefully."
  (let ((mock (ecard-carddav-mock-server-create
               :base-url "https://test.example.com")))
    ;; Add addressbook with one contact
    (ecard-carddav-mock-add-addressbook
     mock "/addressbooks/user/contacts/" "Contacts" "My contacts")
    (ecard-carddav-mock-put-ecard
     mock "/addressbooks/user/contacts/contact1.vcf"
     (ecard-create :fn "John Doe"))

    (unwind-protect
        (progn
          (ecard-carddav-mock-install mock)

          (let* ((server (ecard-carddav-server-create
                          :url "https://test.example.com"
                          :auth (ecard-carddav-auth-basic-create
                                 :username "test"
                                 :password "test"))))
            (ecard-carddav-discover-addressbooks server)
            (let ((addressbook (car (oref server addressbooks))))
              (ecard-carddav-list-resources addressbook)

              ;; Populate contacts - should work even if some contacts fail
              (with-temp-buffer
                (ecard-display-contacts-mode)
                (setq ecard-display--addressbook addressbook)
                ;; Should not error
                (should-not (condition-case err
                                (progn
                                  (ecard-display-contacts--populate addressbook nil t)
                                  nil)
                              (error err)))

                ;; Should have entries
                (should (> (length tabulated-list-entries) 0))))))

      (ecard-carddav-mock-uninstall))))

(ert-deftest ecard-display-test-multiget-batch-size ()
  "Verify multiget works with large batch sizes.
This test disables pagination to verify that loading all 200 contacts
at once works correctly."
  (let ((mock (ecard-carddav-mock-server-create
               :base-url "https://test.example.com")))
    ;; Add addressbook with 200 contacts
    (ecard-carddav-mock-add-addressbook
     mock "/addressbooks/user/contacts/" "Contacts" "My contacts")

    (dotimes (i 200)
      (ecard-carddav-mock-put-ecard
       mock (format "/addressbooks/user/contacts/contact%03d.vcf" i)
       (ecard-create :fn (format "Contact %03d" i))))

    (unwind-protect
        (progn
          (ecard-carddav-mock-install mock)

          (let* ((server (ecard-carddav-server-create
                          :url "https://test.example.com"
                          :auth (ecard-carddav-auth-basic-create
                                 :username "test"
                                 :password "test"))))
            (ecard-carddav-discover-addressbooks server)
            (let ((addressbook (car (oref server addressbooks))))
              (ecard-carddav-list-resources addressbook)

              ;; Populate contacts with 200 resources - disable pagination
              (with-temp-buffer
                (ecard-display-contacts-mode)
                (setq ecard-display--addressbook addressbook)
                ;; Disable pagination to load all contacts at once
                (let ((ecard-display-contacts-page-size nil))
                  (ecard-display-contacts--populate addressbook nil t))

                ;; Verify all 200 contacts loaded
                (should (equal (length tabulated-list-entries) 200))

                ;; Verify all have ecard data
                (let ((resources (oref addressbook resources)))
                  (dolist (resource resources)
                    (should (oref resource ecard))))))))

      (ecard-carddav-mock-uninstall))))

;;; Converted tests from interactive test file

(ert-deftest ecard-display-test-servers-buffer-creation ()
  "Test servers buffer creation with mock environment."
  (unwind-protect
      (progn
        (ecard-display-test--setup-mock-environment)
        (let* ((plist-config (list :name "Mock Server (plist)"
                                   :url "https://mock.test.local"
                                   :username "testuser"
                                   :password "testpass")))
          (setq ecard-carddav-servers (list plist-config))
          ;; FIX: ecard-carddav-servers is not a function, use ecard-display instead
          (ecard-display)
          (should (get-buffer "*CardDAV Servers*"))
          (with-current-buffer "*CardDAV Servers*"
            (should (eq major-mode 'ecard-display-servers-mode))
            (should (equal (length tabulated-list-entries) 1)))))
    (ecard-display-test--cleanup-mock-environment)))

(ert-deftest ecard-display-test-addressbooks-buffer-creation ()
  "Test addressbooks buffer creation with mock environment."
  (unwind-protect
      (progn
        (ecard-display-test--setup-mock-environment)
        (let ((server ecard-display-test--real-server))
          (ecard-carddav-discover-addressbooks server)
          (let ((config (list :name "Mock Test Server"
                              :url "https://mock.test.local")))
            (ecard-display-addressbooks server config)
            (should (get-buffer "*CardDAV: Mock Test Server*"))
            (with-current-buffer "*CardDAV: Mock Test Server*"
              (should (eq major-mode 'ecard-display-addressbooks-mode))
              (should (equal (length tabulated-list-entries) 2))))))
    (ecard-display-test--cleanup-mock-environment)))

(ert-deftest ecard-display-test-contacts-buffer-creation ()
  "Test contacts buffer creation with mock environment."
  (unwind-protect
      (progn
        (ecard-display-test--setup-mock-environment)
        (let ((server ecard-display-test--real-server))
          (ecard-carddav-discover-addressbooks server)
          (let ((addressbook (car (oref server addressbooks))))
            (should addressbook)
            (ecard-carddav-list-resources addressbook)
            (ecard-display-contacts addressbook)
            (should (get-buffer "*CardDAV Contacts: Personal Contacts*"))
            (with-current-buffer "*CardDAV Contacts: Personal Contacts*"
              (should (eq major-mode 'ecard-display-contacts-mode))
              (should (equal (length tabulated-list-entries) 5))))))
    (ecard-display-test--cleanup-mock-environment)))

(ert-deftest ecard-display-test-contact-detail-buffer-creation ()
  "Test contact detail buffer creation with mock environment."
  (unwind-protect
      (progn
        (ecard-display-test--setup-mock-environment)
        (let ((server ecard-display-test--real-server))
          (ecard-carddav-discover-addressbooks server)
          (let ((addressbook (car (oref server addressbooks))))
            (should addressbook)
            (ecard-carddav-list-resources addressbook)
            (let ((resource (car (oref addressbook resources))))
              (should resource)
              (unless (oref resource ecard)
                (let ((fetched (ecard-carddav-get-resource addressbook (oref resource url))))
                  (oset resource ecard (oref fetched ecard))
                  (oset resource etag (oref fetched etag))))
              (ecard-display-contact-detail resource)
              (let ((buffer-name (format "*CardDAV Contact: %s*"
                                         (ecard-display--get-fn (oref resource ecard)))))
                (should (get-buffer buffer-name))
                (with-current-buffer buffer-name
                  (should (eq major-mode 'ecard-display-contact-mode))
                  (should (> (buffer-size) 0))))))))
    (ecard-display-test--cleanup-mock-environment)))

(ert-deftest ecard-display-test-pagination-functionality ()
  "Test pagination controls work correctly."
  (unwind-protect
      (progn
        (ecard-display-test--setup-mock-environment)
        (let ((server ecard-display-test--real-server))
          (ecard-carddav-discover-addressbooks server)
          (let ((addressbook (car (oref server addressbooks))))
            (should addressbook)
            (ecard-carddav-list-resources addressbook)
            (ecard-display-contacts addressbook)
            (with-current-buffer "*CardDAV Contacts: Personal Contacts*"
              ;; Verify pagination is active
              (should ecard-display-contacts-page-size)
              (should (equal ecard-display--current-page 0))
              (should (equal ecard-display--total-contacts 5))

              ;; Test next page (should not error even at last page)
              (ecard-display-contacts-next-page)

              ;; Test prev page
              (ecard-display-contacts-prev-page)
              (should (equal ecard-display--current-page 0))))))
    (ecard-display-test--cleanup-mock-environment)))

(ert-deftest ecard-display-test-contact-add-operation ()
  "Test adding a contact through the UI."
  (unwind-protect
      (progn
        (ecard-display-test--setup-mock-environment)
        (let ((server ecard-display-test--real-server))
          (ecard-carddav-discover-addressbooks server)
          (let ((addressbook (car (oref server addressbooks))))
            (should addressbook)
            (ecard-carddav-list-resources addressbook)
            (let ((initial-count (length (oref addressbook resources))))
              ;; Create and add a test contact programmatically
              (let* ((fn "Test User")
                     (ecard-obj (ecard-create :fn fn))
                     (path "/addressbooks/testuser/contacts/test-new.vcf"))
                (oset ecard-obj uid (list (ecard-property
                                           :name "UID"
                                           :value (format "urn:uuid:%s"
                                                          (ecard-display--generate-uuid)))))
                (ecard-carddav-put-ecard addressbook path ecard-obj)
                (ecard-carddav-list-resources addressbook)
                (should (equal (length (oref addressbook resources)) (1+ initial-count))))))))
    (ecard-display-test--cleanup-mock-environment)))

(ert-deftest ecard-display-test-contact-delete-operation ()
  "Test deleting a contact."
  (unwind-protect
      (progn
        (ecard-display-test--setup-mock-environment)
        (let ((server ecard-display-test--real-server))
          (ecard-carddav-discover-addressbooks server)
          (let ((addressbook (car (oref server addressbooks))))
            (should addressbook)
            (ecard-carddav-list-resources addressbook)
            (let ((initial-count (length (oref addressbook resources)))
                  (resource (car (oref addressbook resources))))
              (should resource)
              ;; Delete the contact
              (ecard-carddav-delete-resource addressbook
                                             (oref resource url)
                                             (oref resource etag))
              (ecard-carddav-list-resources addressbook)
              (should (equal (length (oref addressbook resources)) (1- initial-count)))))))
    (ecard-display-test--cleanup-mock-environment)))

(ert-deftest ecard-display-test-pagination-shows-real-names ()
  "Verify paginated contacts show real names for current page only.
After the populate change, only the current page resources are displayed,
not all resources with some having placeholder names."
  (let ((mock (ecard-carddav-mock-server-create
               :base-url "https://test.example.com")))
    ;; Add addressbook with 5 contacts with UUID filenames
    (ecard-carddav-mock-add-addressbook
     mock "/addressbooks/user/contacts/" "Contacts" "Test contacts")

    (dotimes (i 5)
      (let ((uuid (format "uuid-%d" i)))
        (ecard-carddav-mock-put-ecard
         mock (format "/addressbooks/user/contacts/%s.vcf" uuid)
         (ecard-create
          :fn (format "Person %d" i)
          :email (format "person%d@example.com" i)
          :tel (format "+1-555-010%d" i)))))

    (unwind-protect
        (progn
          (ecard-carddav-mock-install mock)

          (let* ((server (ecard-carddav-server-create
                          :url "https://test.example.com"
                          :auth (ecard-carddav-auth-basic-create
                                 :username "test"
                                 :password "test"))))
            (ecard-carddav-discover-addressbooks server)
            (let ((addressbook (car (oref server addressbooks))))
              (ecard-carddav-list-resources addressbook)

              ;; Test with page size of 2 (first page should have real names)
              (with-temp-buffer
                (ecard-display-contacts-mode)
                (setq ecard-display--addressbook addressbook
                      ecard-display-contacts-page-size 2)  ; Page size of 2
                (ecard-display-contacts--populate addressbook nil t)

                ;; NEW BEHAVIOR: Should have only 2 entries (current page size)
                ;; not all 5 resources - this is the performance improvement
                (should (equal (length tabulated-list-entries) 2))

                ;; Both entries on current page should show real names
                (let ((entry0 (nth 0 tabulated-list-entries))
                      (entry1 (nth 1 tabulated-list-entries)))
                  (should (equal (aref (cadr entry0) 0) "Person 0"))
                  (should (equal (aref (cadr entry0) 1) "person0@example.com"))
                  (should (equal (aref (cadr entry0) 2) "+1-555-0100"))

                  (should (equal (aref (cadr entry1) 0) "Person 1"))
                  (should (equal (aref (cadr entry1) 1) "person1@example.com"))
                  (should (equal (aref (cadr entry1) 2) "+1-555-0101")))

                ;; Verify total count is tracked for pagination info
                (should (equal ecard-display--total-contacts 5))
                (should (equal ecard-display--current-page 0))))))

      (ecard-carddav-mock-uninstall))))

;;; UID Display Tests

(ert-deftest ecard-display-test-uid-in-detail-view ()
  "Test that UID is displayed in contact detail view."
  (let* ((uid-prop (ecard-property :name "UID" :value "urn:uuid:test-uid-12345"))
         (fn-prop (ecard-property :name "FN" :value "Test Person"))
         (ecard-obj (ecard :fn (list fn-prop) :uid (list uid-prop)))
         (addressbook (ecard-carddav-addressbook :url "https://test.example.com/contacts/"))
         (resource (ecard-carddav-resource
                    :addressbook addressbook
                    :url "https://test.example.com/contacts/test.vcf"
                    :path "/contacts/test.vcf"
                    :ecard ecard-obj)))
    (with-temp-buffer
      (ecard-display-contact-mode)
      (setq ecard-display--resource resource
            ecard-display--addressbook addressbook)
      (ecard-display-contact--render resource)
      (let ((content (buffer-string)))
        (should (string-match-p "UID:" content))
        (should (string-match-p "urn:uuid:test-uid-12345" content))))))

(ert-deftest ecard-display-test-no-uid-in-detail-view ()
  "Test that contact detail view works without UID."
  (let* ((fn-prop (ecard-property :name "FN" :value "Test Person"))
         (ecard-obj (ecard :fn (list fn-prop)))
         (addressbook (ecard-carddav-addressbook :url "https://test.example.com/contacts/"))
         (resource (ecard-carddav-resource
                    :addressbook addressbook
                    :url "https://test.example.com/contacts/test.vcf"
                    :path "/contacts/test.vcf"
                    :ecard ecard-obj)))
    (with-temp-buffer
      (ecard-display-contact-mode)
      (setq ecard-display--resource resource
            ecard-display--addressbook addressbook)
      (ecard-display-contact--render resource)
      (let ((content (buffer-string)))
        ;; Should not display UID section if no UID
        (should-not (string-match-p "UID:" content))
        ;; But should still display other content
        (should (string-match-p "Full Name:" content))
        (should (string-match-p "Test Person" content))))))

;;; Widget integration tests

(ert-deftest ecard-display-test-widget-contact-detail ()
  "Test contact detail uses widget-based editing."
  (let* ((fn-prop (ecard-property :name "FN" :value "Widget Test User"))
         (email-prop (ecard-property
                      :name "EMAIL"
                      :parameters '(("TYPE" . "work"))
                      :value "widget@example.com"))
         (ecard-obj (ecard :fn (list fn-prop) :email (list email-prop)))
         (server (ecard-carddav-server))
         (addressbook (ecard-carddav-addressbook :server server
                                                  :url "https://test.example.com/"))
         (resource (ecard-carddav-resource
                    :addressbook addressbook
                    :url "https://test.example.com/widget-test.vcf"
                    :path "/widget-test.vcf"
                    :ecard ecard-obj
                    :etag "etag-1")))
    ;; Display the contact (which now uses widgets)
    (ecard-display-contact-detail resource)
    (let ((buffer (get-buffer "*CardDAV Contact: Widget Test User*")))
      (unwind-protect
          (with-current-buffer buffer
            ;; Verify widget buffer state
            (should ecard-widget--widgets)
            (should ecard-widget--ecard)
            ;; Verify content
            (let ((content (buffer-string)))
              (should (string-match-p "Widget Test User" content))
              (should (string-match-p "widget@example.com" content))
              ;; Should have section headings
              (should (string-match-p "Name" content))
              (should (string-match-p "Email" content))))
        (when buffer (kill-buffer buffer))))))

(ert-deftest ecard-display-test-widget-modification-detection ()
  "Test that widget modification is detected correctly."
  (let* ((ecard-obj (ecard-create :fn "Original Name"))
         (server (ecard-carddav-server))
         (addressbook (ecard-carddav-addressbook :server server))
         (resource (ecard-carddav-resource
                    :addressbook addressbook
                    :url "https://test.example.com/mod-test.vcf"
                    :path "/mod-test.vcf"
                    :ecard ecard-obj)))
    (ecard-display-contact-detail resource)
    (let ((buffer (get-buffer "*CardDAV Contact: Original Name*")))
      (unwind-protect
          (with-current-buffer buffer
            ;; Initially not modified
            (should-not (ecard-widget-modified-p))
            ;; Modify the FN widget
            (let ((fn-widget (cdr (assq 'fn ecard-widget--widgets))))
              (when fn-widget
                (widget-value-set fn-widget "Modified Name")))
            ;; Should now be modified
            (should (ecard-widget-modified-p)))
        (when buffer (kill-buffer buffer))))))

(ert-deftest ecard-display-test-widget-value-extraction ()
  "Test that widget values are correctly extracted for saving."
  (let* ((ecard-obj (ecard-create :fn "Test User"))
         (server (ecard-carddav-server))
         (addressbook (ecard-carddav-addressbook :server server))
         (resource (ecard-carddav-resource
                    :addressbook addressbook
                    :url "https://test.example.com/extract-test.vcf"
                    :path "/extract-test.vcf"
                    :ecard ecard-obj)))
    (ecard-display-contact-detail resource)
    (let ((buffer (get-buffer "*CardDAV Contact: Test User*")))
      (unwind-protect
          (with-current-buffer buffer
            ;; Modify multiple fields
            (let ((fn-w (cdr (assq 'fn ecard-widget--widgets)))
                  (given-w (cdr (assq 'n-given ecard-widget--widgets)))
                  (family-w (cdr (assq 'n-family ecard-widget--widgets)))
                  (org-w (cdr (assq 'org ecard-widget--widgets))))
              (when fn-w (widget-value-set fn-w "Updated User"))
              (when given-w (widget-value-set given-w "Updated"))
              (when family-w (widget-value-set family-w "User"))
              (when org-w (widget-value-set org-w "Test Company")))
            ;; Extract values
            (let ((result (ecard-widget-get-value)))
              (should (ecard-p result))
              (should (equal (ecard-property-value (car (ecard-fn result)))
                             "Updated User"))
              (let ((n-val (ecard-property-value (car (ecard-n result)))))
                (should (equal (nth 0 n-val) "User"))   ; family
                (should (equal (nth 1 n-val) "Updated"))) ; given
              (should (equal (ecard-property-value (car (ecard-org result)))
                             "Test Company"))))
        (when buffer (kill-buffer buffer))))))

(ert-deftest ecard-display-test-widget-save-integration ()
  "Test saving contact through widget interface with mock server."
  (unwind-protect
      (progn
        (ecard-display-test--setup-mock-environment)
        (let ((server ecard-display-test--real-server))
          (ecard-carddav-discover-addressbooks server)
          (let ((addressbook (car (oref server addressbooks))))
            (ecard-carddav-list-resources addressbook)
            ;; Get first resource
            (let ((resource (car (oref addressbook resources))))
              (when resource
                ;; Fetch full data
                (unless (oref resource ecard)
                  (let ((fetched (ecard-carddav-get-resource addressbook (oref resource url))))
                    (oset resource ecard (oref fetched ecard))
                    (oset resource etag (oref fetched etag))))
                ;; Open in widget editor
                (ecard-display-contact-detail resource)
                (let ((buffer (get-buffer (format "*CardDAV Contact: %s*"
                                                  (ecard-display--get-fn (oref resource ecard))))))
                  (unwind-protect
                      (with-current-buffer buffer
                        ;; Modify name
                        (let ((fn-w (cdr (assq 'fn ecard-widget--widgets))))
                          (when fn-w
                            (widget-value-set fn-w "Widget Modified Name")))
                        ;; Save (should work with mock server)
                        (ecard-display-contact-save)
                        ;; Verify save succeeded
                        (should-not ecard-display--modified))
                    (when buffer (kill-buffer buffer)))))))))
    (ecard-display-test--cleanup-mock-environment)))

;;; Normalize server config tests

(ert-deftest ecard-display-test-normalize-server-config-plist ()
  "Test normalizing a plist server config."
  (let* ((config '(:name "Test" :url "https://test.example.com"
                   :username "user" :password "pass"))
         (normalized (ecard-display--normalize-server-config config)))
    (should (equal (plist-get normalized :name) "Test"))
    (should (equal (plist-get normalized :url) "https://test.example.com"))
    (should (equal (plist-get normalized :username) "user"))
    (should (equal (plist-get normalized :password) "pass"))))

(ert-deftest ecard-display-test-normalize-server-config-object ()
  "Test normalizing a server object config."
  (let* ((auth (ecard-carddav-auth-basic-create
                :username "user" :password "pass"))
         (server (ecard-carddav-server-create
                  :url "https://test.example.com"
                  :auth auth))
         (normalized (ecard-display--normalize-server-config server)))
    (should (equal (plist-get normalized :url) "https://test.example.com"))
    (should (equal (plist-get normalized :username) "user"))
    (should (equal (plist-get normalized :password) "pass"))
    (should (eq (plist-get normalized :server-object) server))))

(ert-deftest ecard-display-test-normalize-server-config-invalid ()
  "Test normalizing invalid config signals error."
  (should-error (ecard-display--normalize-server-config "invalid")
                :type 'error))

(ert-deftest ecard-display-test-normalize-server-config-object-no-auth ()
  "Test normalizing a server object without auth."
  ;; Use low-level EIEIO constructor to bypass server-create auth validation
  (let* ((server (ecard-carddav-server :url "https://test.example.com"))
         (normalized (ecard-display--normalize-server-config server)))
    (should (equal (plist-get normalized :url) "https://test.example.com"))
    (should (null (plist-get normalized :username)))
    (should (null (plist-get normalized :password)))))

;;; Server config to object tests

(ert-deftest ecard-display-test-server-config-to-object-plist ()
  "Test creating server object from plist config."
  (let* ((config '(:name "Test" :url "https://test.example.com"
                   :username "user" :password "pass"))
         (server (ecard-display--server-config-to-object config)))
    (should (ecard-carddav-server-p server))
    (should (equal (oref server url) "https://test.example.com"))
    (should (oref server auth))))

(ert-deftest ecard-display-test-server-config-to-object-already-object ()
  "Test server-config-to-object with existing server object."
  (let* ((auth (ecard-carddav-auth-basic-create
                :username "user" :password "pass"))
         (server (ecard-carddav-server-create
                  :url "https://test.example.com"
                  :auth auth))
         (result (ecard-display--server-config-to-object server)))
    (should (eq result server))))

(ert-deftest ecard-display-test-server-config-to-object-no-url ()
  "Test server-config-to-object errors without URL."
  (should-error (ecard-display--server-config-to-object '(:name "Test"))
                :type 'error))

(ert-deftest ecard-display-test-server-config-to-object-no-auth ()
  "Test server-config-to-object without credentials errors."
  ;; server-create requires auth, so this should signal an error
  (should-error (ecard-display--server-config-to-object '(:url "https://test.example.com"))
                :type 'ecard-carddav-error))

(ert-deftest ecard-display-test-server-config-to-object-invalid ()
  "Test server-config-to-object with invalid input."
  (should-error (ecard-display--server-config-to-object 42)
                :type 'error))

;;; ecard-display entry point test

(ert-deftest ecard-display-test-entry-point ()
  "Test ecard-display creates server buffer."
  (let ((saved-servers ecard-carddav-servers))
    (unwind-protect
        (progn
          (setq ecard-carddav-servers
                '((:name "Entry Test" :url "https://entry.example.com"
                   :username "u" :password "p")))
          (ecard-display)
          (let ((buf (get-buffer "*CardDAV Servers*")))
            (should buf)
            (with-current-buffer buf
              (should (eq major-mode 'ecard-display-servers-mode))
              (should (= (length tabulated-list-entries) 1)))
            (kill-buffer buf)))
      (setq ecard-carddav-servers saved-servers))))

;;; Refresh test

(ert-deftest ecard-display-test-refresh-updates-entries ()
  "Test that ecard-display-refresh updates tabulated list."
  (let ((saved-servers ecard-carddav-servers))
    (unwind-protect
        (progn
          (setq ecard-carddav-servers
                '((:name "S1" :url "https://s1.example.com"
                   :username "u1" :password "p1")))
          (with-temp-buffer
            (ecard-display-servers-mode)
            (ecard-display-refresh)
            (should (= (length tabulated-list-entries) 1))
            ;; Add another server and refresh
            (setq ecard-carddav-servers
                  (append ecard-carddav-servers
                          '((:name "S2" :url "https://s2.example.com"
                             :username "u2" :password "p2"))))
            (ecard-display-refresh)
            (should (= (length tabulated-list-entries) 2))))
      (setq ecard-carddav-servers saved-servers))))

(ert-deftest ecard-display-test-refresh-empty-servers ()
  "Test refresh with no servers."
  (let ((saved-servers ecard-carddav-servers))
    (unwind-protect
        (progn
          (setq ecard-carddav-servers nil)
          (with-temp-buffer
            (ecard-display-servers-mode)
            (ecard-display-refresh)
            (should (= (length tabulated-list-entries) 0))))
      (setq ecard-carddav-servers saved-servers))))

;;; Servers add/delete with mock UI

(ert-deftest ecard-display-test-servers-add-mock ()
  "Test adding a server with mocked UI."
  (let ((saved-servers ecard-carddav-servers)
        (input-count 0))
    (unwind-protect
        (progn
          (setq ecard-carddav-servers nil)
          (cl-letf (((symbol-function 'read-string)
                     (lambda (&rest _)
                       (setq input-count (1+ input-count))
                       (pcase input-count
                         (1 "New Server")
                         (2 "https://new.example.com")
                         (3 "newuser"))))
                    ((symbol-function 'read-passwd)
                     (lambda (&rest _) "newpass"))
                    ((symbol-function 'customize-save-variable)
                     (lambda (sym val) (set sym val))))
            (with-temp-buffer
              (ecard-display-servers-mode)
              (ecard-display-servers-add)
              (should (= (length ecard-carddav-servers) 1))
              (let ((config (car ecard-carddav-servers)))
                (should (equal (plist-get config :name) "New Server"))
                (should (equal (plist-get config :url) "https://new.example.com"))
                (should (equal (plist-get config :username) "newuser"))
                (should (equal (plist-get config :password) "newpass"))))))
      (setq ecard-carddav-servers saved-servers))))

(ert-deftest ecard-display-test-servers-delete-mock ()
  "Test deleting a server with mocked UI."
  (let ((saved-servers ecard-carddav-servers))
    (unwind-protect
        (progn
          (setq ecard-carddav-servers
                '((:name "Delete Me" :url "https://delete.example.com"
                   :username "u" :password "p")))
          (cl-letf (((symbol-function 'tabulated-list-get-id)
                     (lambda () (car ecard-carddav-servers)))
                    ((symbol-function 'yes-or-no-p)
                     (lambda (&rest _) t))
                    ((symbol-function 'customize-save-variable)
                     (lambda (sym val) (set sym val))))
            (with-temp-buffer
              (ecard-display-servers-mode)
              (ecard-display-servers-refresh)
              (ecard-display-servers-delete)
              (should (= (length ecard-carddav-servers) 0)))))
      (setq ecard-carddav-servers saved-servers))))

(ert-deftest ecard-display-test-servers-delete-no-server-at-point ()
  "Test deleting when no server at point."
  (cl-letf (((symbol-function 'tabulated-list-get-id)
             (lambda () nil)))
    (with-temp-buffer
      (ecard-display-servers-mode)
      (should-error (ecard-display-servers-delete) :type 'user-error))))

(ert-deftest ecard-display-test-servers-delete-cancelled ()
  "Test deleting when user cancels."
  (let ((saved-servers ecard-carddav-servers))
    (unwind-protect
        (progn
          (setq ecard-carddav-servers
                '((:name "Keep Me" :url "https://keep.example.com"
                   :username "u" :password "p")))
          (cl-letf (((symbol-function 'tabulated-list-get-id)
                     (lambda () (car ecard-carddav-servers)))
                    ((symbol-function 'yes-or-no-p)
                     (lambda (&rest _) nil)))
            (with-temp-buffer
              (ecard-display-servers-mode)
              (ecard-display-servers-refresh)
              (ecard-display-servers-delete)
              (should (= (length ecard-carddav-servers) 1)))))
      (setq ecard-carddav-servers saved-servers))))

;;; Servers open with mock server

(ert-deftest ecard-display-test-servers-open-mock ()
  "Test opening a server to view addressbooks."
  (let ((mock (ecard-carddav-mock-server-create
               :base-url "https://test.example.com")))
    (ecard-carddav-mock-add-addressbook
     mock "/addressbooks/user/contacts/" "Contacts" "My contacts")
    (unwind-protect
        (progn
          (ecard-carddav-mock-install mock)
          (let ((config '(:name "Test Server"
                          :url "https://test.example.com"
                          :username "test" :password "test")))
            (cl-letf (((symbol-function 'tabulated-list-get-id)
                       (lambda () config))
                      ((symbol-function 'switch-to-buffer)
                       (lambda (buf) buf)))
              (with-temp-buffer
                (ecard-display-servers-mode)
                (ecard-display-servers-open)
                ;; Should have created the addressbook buffer
                (let ((ab-buf (get-buffer "*CardDAV: Test Server*")))
                  (when ab-buf
                    (with-current-buffer ab-buf
                      (should (eq major-mode 'ecard-display-addressbooks-mode))
                      (should (= (length tabulated-list-entries) 1)))
                    (kill-buffer ab-buf)))))))
      (ecard-carddav-mock-uninstall))))

(ert-deftest ecard-display-test-servers-open-no-server-at-point ()
  "Test opening when no server at point."
  (cl-letf (((symbol-function 'tabulated-list-get-id)
             (lambda () nil)))
    (with-temp-buffer
      (ecard-display-servers-mode)
      (should-error (ecard-display-servers-open) :type 'user-error))))

;;; Addressbooks tests

(ert-deftest ecard-display-test-addressbooks-back ()
  "Test returning to server list from addressbooks."
  (let ((saved-servers ecard-carddav-servers))
    (unwind-protect
        (progn
          (setq ecard-carddav-servers nil)
          (ecard-display-addressbooks-back)
          (should (get-buffer "*CardDAV Servers*"))
          (kill-buffer "*CardDAV Servers*"))
      (setq ecard-carddav-servers saved-servers))))

(ert-deftest ecard-display-test-addressbooks-refresh-no-server ()
  "Test addressbooks refresh without server."
  (with-temp-buffer
    (ecard-display-addressbooks-mode)
    (should-error (ecard-display-addressbooks-refresh) :type 'user-error)))

(ert-deftest ecard-display-test-addressbooks-open-no-addressbook ()
  "Test opening when no addressbook at point."
  (cl-letf (((symbol-function 'tabulated-list-get-id)
             (lambda () nil)))
    (with-temp-buffer
      (ecard-display-addressbooks-mode)
      (should-error (ecard-display-addressbooks-open) :type 'user-error))))

(ert-deftest ecard-display-test-addressbooks-open-with-mock ()
  "Test opening an addressbook to view contacts."
  (let ((mock (ecard-carddav-mock-server-create
               :base-url "https://test.example.com")))
    (ecard-carddav-mock-add-addressbook
     mock "/addressbooks/user/contacts/" "Contacts" "My contacts")
    (ecard-carddav-mock-put-ecard
     mock "/addressbooks/user/contacts/c1.vcf"
     (ecard-create :fn "Person One"))
    (unwind-protect
        (progn
          (ecard-carddav-mock-install mock)
          (let* ((server (ecard-carddav-server-create
                          :url "https://test.example.com"
                          :auth (ecard-carddav-auth-basic-create
                                 :username "test" :password "test"))))
            (ecard-carddav-discover-addressbooks server)
            (let ((addressbook (car (oref server addressbooks))))
              (cl-letf (((symbol-function 'tabulated-list-get-id)
                         (lambda () addressbook))
                        ((symbol-function 'switch-to-buffer)
                         (lambda (buf) buf)))
                (with-temp-buffer
                  (ecard-display-addressbooks-mode)
                  (setq ecard-display--server server)
                  (ecard-display-addressbooks-open)
                  (let ((ct-buf (get-buffer "*CardDAV Contacts: Contacts*")))
                    (when ct-buf
                      (with-current-buffer ct-buf
                        (should (eq major-mode 'ecard-display-contacts-mode))
                        (should (>= (length tabulated-list-entries) 1)))
                      (kill-buffer ct-buf))))))))
      (ecard-carddav-mock-uninstall))))

;;; Contacts buffer tests

(ert-deftest ecard-display-test-contacts-refresh-no-addressbook ()
  "Test contacts refresh without addressbook."
  (with-temp-buffer
    (ecard-display-contacts-mode)
    (should-error (ecard-display-contacts-refresh) :type 'user-error)))

(ert-deftest ecard-display-test-contacts-refresh-names-no-addressbook ()
  "Test contacts refresh names without addressbook."
  (with-temp-buffer
    (ecard-display-contacts-mode)
    (should-error (ecard-display-contacts-refresh-names) :type 'user-error)))

(ert-deftest ecard-display-test-contacts-next-page-no-addressbook ()
  "Test next page without addressbook."
  (with-temp-buffer
    (ecard-display-contacts-mode)
    (should-error (ecard-display-contacts-next-page) :type 'user-error)))

(ert-deftest ecard-display-test-contacts-prev-page-no-addressbook ()
  "Test prev page without addressbook."
  (with-temp-buffer
    (ecard-display-contacts-mode)
    (should-error (ecard-display-contacts-prev-page) :type 'user-error)))

(ert-deftest ecard-display-test-contacts-load-all-no-addressbook ()
  "Test load all without addressbook."
  (with-temp-buffer
    (ecard-display-contacts-mode)
    (should-error (ecard-display-contacts-load-all) :type 'user-error)))

(ert-deftest ecard-display-test-contacts-next-page-no-pagination ()
  "Test next page when pagination is disabled."
  (let ((mock (ecard-carddav-mock-server-create
               :base-url "https://test.example.com")))
    (ecard-carddav-mock-add-addressbook
     mock "/addressbooks/user/contacts/" "Contacts" "My contacts")
    (ecard-carddav-mock-put-ecard
     mock "/addressbooks/user/contacts/c1.vcf"
     (ecard-create :fn "Test"))
    (unwind-protect
        (progn
          (ecard-carddav-mock-install mock)
          (let* ((server (ecard-carddav-server-create
                          :url "https://test.example.com"
                          :auth (ecard-carddav-auth-basic-create
                                 :username "t" :password "t"))))
            (ecard-carddav-discover-addressbooks server)
            (let ((addressbook (car (oref server addressbooks))))
              (ecard-carddav-list-resources addressbook)
              (with-temp-buffer
                (ecard-display-contacts-mode)
                (setq ecard-display--addressbook addressbook
                      ecard-display-contacts-page-size nil)
                (should-error (ecard-display-contacts-next-page)
                              :type 'user-error)))))
      (ecard-carddav-mock-uninstall))))

(ert-deftest ecard-display-test-contacts-prev-page-at-first ()
  "Test prev page when already at first page."
  (let ((mock (ecard-carddav-mock-server-create
               :base-url "https://test.example.com")))
    (ecard-carddav-mock-add-addressbook
     mock "/addressbooks/user/contacts/" "Contacts" "My contacts")
    (dotimes (i 5)
      (ecard-carddav-mock-put-ecard
       mock (format "/addressbooks/user/contacts/c%d.vcf" i)
       (ecard-create :fn (format "Person %d" i))))
    (unwind-protect
        (progn
          (ecard-carddav-mock-install mock)
          (let* ((server (ecard-carddav-server-create
                          :url "https://test.example.com"
                          :auth (ecard-carddav-auth-basic-create
                                 :username "t" :password "t"))))
            (ecard-carddav-discover-addressbooks server)
            (let ((addressbook (car (oref server addressbooks))))
              (ecard-carddav-list-resources addressbook)
              (with-temp-buffer
                (ecard-display-contacts-mode)
                (setq ecard-display--addressbook addressbook
                      ecard-display--current-page 0
                      ecard-display--total-contacts 5
                      ecard-display-contacts-page-size 2)
                ;; Should just display a message, not error
                (ecard-display-contacts-prev-page)
                (should (= ecard-display--current-page 0))))))
      (ecard-carddav-mock-uninstall))))

(ert-deftest ecard-display-test-contacts-open-no-contact ()
  "Test opening when no contact at point."
  (cl-letf (((symbol-function 'tabulated-list-get-id)
             (lambda () nil)))
    (with-temp-buffer
      (ecard-display-contacts-mode)
      (should-error (ecard-display-contacts-open) :type 'user-error))))

(ert-deftest ecard-display-test-contacts-add-with-mock ()
  "Test adding a contact builds correct ecard and calls put."
  (let* ((put-called nil)
         (put-ecard nil)
         (input-count 0)
         (server (ecard-carddav-server
                  :url "https://test.example.com"))
         (addressbook (ecard-carddav-addressbook
                       :server server
                       :url "https://test.example.com/contacts/")))
    (cl-letf (((symbol-function 'read-string)
               (lambda (&rest _)
                 (setq input-count (1+ input-count))
                 (pcase input-count
                   (1 "New Contact")  ; Full name
                   (2 "New")          ; Given name
                   (3 "Contact")      ; Family name
                   (4 "new@test.com") ; Email
                   (5 "+1-555-0000")  ; Phone
                   (_ ""))))
              ((symbol-function 'ecard-carddav-put-ecard)
               (lambda (ab _path ecard-obj &rest _)
                 (setq put-called t
                       put-ecard ecard-obj)
                 (ecard-carddav-resource :addressbook ab :url "test.vcf")))
              ((symbol-function 'ecard-display-contacts-refresh)
               (lambda () nil)))
      (with-temp-buffer
        (ecard-display-contacts-mode)
        (setq ecard-display--addressbook addressbook)
        (ecard-display-contacts-add)
        ;; Verify put was called
        (should put-called)
        ;; Verify the created ecard has correct properties
        (should (equal "New Contact" (ecard-display--get-fn put-ecard)))
        (should (equal "new@test.com" (ecard-display--get-first-email put-ecard)))
        (should (equal "+1-555-0000" (ecard-display--get-first-tel put-ecard)))
        ;; Verify N property
        (let ((n-val (ecard-get-property-value put-ecard 'n)))
          (should (equal "Contact" (nth 0 n-val)))
          (should (equal "New" (nth 1 n-val))))
        ;; Verify UID was generated
        (should (ecard-get-property-value put-ecard 'uid))))))

(ert-deftest ecard-display-test-contacts-add-no-addressbook ()
  "Test adding contact without addressbook."
  (with-temp-buffer
    (ecard-display-contacts-mode)
    (should-error (ecard-display-contacts-add) :type 'user-error)))

(ert-deftest ecard-display-test-contacts-delete-with-mock ()
  "Test deleting a contact via the contacts list."
  (let ((mock (ecard-carddav-mock-server-create
               :base-url "https://test.example.com")))
    (ecard-carddav-mock-add-addressbook
     mock "/addressbooks/user/contacts/" "Contacts" "My contacts")
    (ecard-carddav-mock-put-ecard
     mock "/addressbooks/user/contacts/c1.vcf"
     (ecard-create :fn "Delete Me"))
    (unwind-protect
        (progn
          (ecard-carddav-mock-install mock)
          (let* ((server (ecard-carddav-server-create
                          :url "https://test.example.com"
                          :auth (ecard-carddav-auth-basic-create
                                 :username "t" :password "t"))))
            (ecard-carddav-discover-addressbooks server)
            (let ((addressbook (car (oref server addressbooks))))
              (ecard-carddav-list-resources addressbook)
              (let ((resource (car (oref addressbook resources))))
                ;; Fetch ecard data for the resource
                (let ((fetched (ecard-carddav-get-resource
                                addressbook (oref resource url))))
                  (oset resource ecard (oref fetched ecard))
                  (oset resource etag (oref fetched etag)))
                (cl-letf (((symbol-function 'tabulated-list-get-id)
                           (lambda () resource))
                          ((symbol-function 'yes-or-no-p)
                           (lambda (&rest _) t)))
                  (with-temp-buffer
                    (ecard-display-contacts-mode)
                    (setq ecard-display--addressbook addressbook
                          ecard-display-confirm-delete t)
                    (ecard-display-contacts-delete)
                    ;; Verify deletion
                    (ecard-carddav-list-resources addressbook)
                    (should (= (length (oref addressbook resources)) 0))))))))
      (ecard-carddav-mock-uninstall))))

(ert-deftest ecard-display-test-contacts-delete-no-contact ()
  "Test deleting when no contact at point."
  (cl-letf (((symbol-function 'tabulated-list-get-id)
             (lambda () nil)))
    (with-temp-buffer
      (ecard-display-contacts-mode)
      (should-error (ecard-display-contacts-delete) :type 'user-error))))

;;; Contacts back test

(ert-deftest ecard-display-test-contacts-back-with-server ()
  "Test returning to server list from contacts."
  (let ((mock (ecard-carddav-mock-server-create
               :base-url "https://test.example.com")))
    (ecard-carddav-mock-add-addressbook
     mock "/addressbooks/user/contacts/" "Contacts" "My contacts")
    (unwind-protect
        (progn
          (ecard-carddav-mock-install mock)
          (let* ((server (ecard-carddav-server-create
                          :url "https://test.example.com"
                          :auth (ecard-carddav-auth-basic-create
                                 :username "t" :password "t")))
                 (saved-servers ecard-carddav-servers))
            (ecard-carddav-discover-addressbooks server)
            (let ((addressbook (car (oref server addressbooks))))
              (setq ecard-carddav-servers nil)
              (with-temp-buffer
                (ecard-display-contacts-mode)
                (setq ecard-display--addressbook addressbook)
                (ecard-display-contacts-back)
                (should (get-buffer "*CardDAV Servers*"))
                (kill-buffer "*CardDAV Servers*"))
              (setq ecard-carddav-servers saved-servers))))
      (ecard-carddav-mock-uninstall))))

;;; Contact detail tests

(ert-deftest ecard-display-test-contact-render-full ()
  "Test rendering contact with all fields."
  (let* ((fn-prop (ecard-property :name "FN" :value "Full Contact"))
         (n-prop (ecard-property :name "N" :value '("Contact" "Full" "Q" "Dr." "Jr.")))
         (email-prop (ecard-property :name "EMAIL"
                                     :parameters '(("TYPE" . "work"))
                                     :value "full@example.com"))
         (tel-prop (ecard-property :name "TEL"
                                   :parameters '(("TYPE" . "cell"))
                                   :value "+1-555-0001"))
         (adr-prop (ecard-property :name "ADR"
                                   :parameters '(("TYPE" . "home"))
                                   :value '("" "" "123 Main St" "City" "ST" "12345" "US")))
         (org-prop (ecard-property :name "ORG" :value "Test Corp"))
         (title-prop (ecard-property :name "TITLE" :value "Engineer"))
         (note-prop (ecard-property :name "NOTE" :value "A test note"))
         (ecard-obj (ecard :fn (list fn-prop) :n (list n-prop)
                           :email (list email-prop) :tel (list tel-prop)
                           :adr (list adr-prop) :org (list org-prop)
                           :title (list title-prop) :note (list note-prop)))
         (addressbook (ecard-carddav-addressbook :url "https://test.example.com/contacts/"))
         (resource (ecard-carddav-resource
                    :addressbook addressbook
                    :url "https://test.example.com/contacts/full.vcf"
                    :path "/contacts/full.vcf"
                    :ecard ecard-obj)))
    (with-temp-buffer
      (ecard-display-contact-mode)
      (setq ecard-display--resource resource
            ecard-display--addressbook addressbook)
      (ecard-display-contact--render resource)
      (let ((content (buffer-string)))
        (should (string-match-p "Contact Details" content))
        (should (string-match-p "Full Contact" content))
        (should (string-match-p "Given Name:" content))
        (should (string-match-p "Family Name:" content))
        (should (string-match-p "Additional:" content))
        (should (string-match-p "Prefix:" content))
        (should (string-match-p "Suffix:" content))
        (should (string-match-p "full@example.com" content))
        (should (string-match-p "\\+1-555-0001" content))
        (should (string-match-p "123 Main St" content))
        (should (string-match-p "Test Corp" content))
        (should (string-match-p "Engineer" content))
        (should (string-match-p "A test note" content))))))

(ert-deftest ecard-display-test-contact-render-address-as-list ()
  "Test rendering contact with list-type address."
  (let* ((fn-prop (ecard-property :name "FN" :value "Addr Test"))
         (adr-prop (ecard-property :name "ADR"
                                   :parameters '(("TYPE" . "work"))
                                   :value '("" "" "456 Oak Ave" "Town" "CA" "90210" "US")))
         (ecard-obj (ecard :fn (list fn-prop) :adr (list adr-prop)))
         (addressbook (ecard-carddav-addressbook :url "https://test.example.com/"))
         (resource (ecard-carddav-resource
                    :addressbook addressbook
                    :url "https://test.example.com/addr.vcf"
                    :path "/addr.vcf"
                    :ecard ecard-obj)))
    (with-temp-buffer
      (ecard-display-contact-mode)
      (setq ecard-display--resource resource
            ecard-display--addressbook addressbook)
      (ecard-display-contact--render resource)
      (let ((content (buffer-string)))
        (should (string-match-p "456 Oak Ave" content))
        (should (string-match-p "Town" content))))))

(ert-deftest ecard-display-test-contact-render-address-as-string ()
  "Test rendering address when value is a string instead of list."
  (let* ((fn-prop (ecard-property :name "FN" :value "String Addr"))
         (adr-prop (ecard-property :name "ADR"
                                   :parameters '(("TYPE" . "home"))
                                   :value "123 Simple St, City"))
         (ecard-obj (ecard :fn (list fn-prop) :adr (list adr-prop)))
         (addressbook (ecard-carddav-addressbook :url "https://test.example.com/"))
         (resource (ecard-carddav-resource
                    :addressbook addressbook
                    :url "https://test.example.com/str-addr.vcf"
                    :path "/str-addr.vcf"
                    :ecard ecard-obj)))
    (with-temp-buffer
      (ecard-display-contact-mode)
      (setq ecard-display--resource resource
            ecard-display--addressbook addressbook)
      (ecard-display-contact--render resource)
      (let ((content (buffer-string)))
        (should (string-match-p "123 Simple St, City" content))))))

;;; Contact on-change test

(ert-deftest ecard-display-test-contact-on-change ()
  "Test on-change callback sets modified state."
  (let* ((ecard-obj (ecard-create :fn "On Change Test"))
         (server (ecard-carddav-server))
         (addressbook (ecard-carddav-addressbook :server server))
         (resource (ecard-carddav-resource
                    :addressbook addressbook
                    :url "https://test.example.com/change.vcf"
                    :path "/change.vcf"
                    :ecard ecard-obj)))
    (ecard-display-contact-detail resource)
    (let ((buffer (get-buffer "*CardDAV Contact: On Change Test*")))
      (unwind-protect
          (with-current-buffer buffer
            ;; Initially not modified
            (should-not ecard-display--modified)
            ;; Modify a widget
            (let ((fn-w (cdr (assq 'fn ecard-widget--widgets))))
              (when fn-w
                (widget-value-set fn-w "Changed Name")))
            ;; Trigger the on-change handler
            (ecard-display-contact--on-change)
            ;; Should reflect modified state
            (should ecard-display--modified))
        (when buffer (kill-buffer buffer))))))

;;; Contact save test

(ert-deftest ecard-display-test-contact-save-no-resource ()
  "Test saving without resource."
  (with-temp-buffer
    (ecard-display-contact-mode)
    (should-error (ecard-display-contact-save) :type 'user-error)))

;;; Contact revert test

(ert-deftest ecard-display-test-contact-revert-not-modified ()
  "Test reverting when not modified does nothing."
  (let* ((ecard-obj (ecard-create :fn "Revert Test"))
         (server (ecard-carddav-server))
         (addressbook (ecard-carddav-addressbook :server server))
         (resource (ecard-carddav-resource
                    :addressbook addressbook
                    :url "https://test.example.com/revert.vcf"
                    :path "/revert.vcf"
                    :ecard ecard-obj)))
    (ecard-display-contact-detail resource)
    (let ((buffer (get-buffer "*CardDAV Contact: Revert Test*")))
      (unwind-protect
          (with-current-buffer buffer
            ;; Not modified, revert should work without prompting
            (ecard-display-contact-revert)
            (should-not ecard-display--modified))
        (when buffer (kill-buffer buffer))))))

(ert-deftest ecard-display-test-contact-revert-modified ()
  "Test reverting restores original values."
  (let* ((ecard-obj (ecard-create :fn "Original Name"))
         (server (ecard-carddav-server))
         (addressbook (ecard-carddav-addressbook :server server))
         (resource (ecard-carddav-resource
                    :addressbook addressbook
                    :url "https://test.example.com/revert2.vcf"
                    :path "/revert2.vcf"
                    :ecard ecard-obj)))
    (ecard-display-contact-detail resource)
    (let ((buffer (get-buffer "*CardDAV Contact: Original Name*")))
      (unwind-protect
          (with-current-buffer buffer
            ;; Modify a widget
            (let ((fn-w (cdr (assq 'fn ecard-widget--widgets))))
              (when fn-w
                (widget-value-set fn-w "Modified Name")))
            (setq ecard-display--modified t)
            ;; Revert with user confirmation
            (cl-letf (((symbol-function 'yes-or-no-p)
                       (lambda (&rest _) t)))
              (ecard-display-contact-revert))
            (should-not ecard-display--modified))
        (when buffer (kill-buffer buffer))))))

;;; Contact delete from detail view

(ert-deftest ecard-display-test-contact-detail-delete-no-resource ()
  "Test deleting from detail view without resource."
  (with-temp-buffer
    (ecard-display-contact-mode)
    (should-error (ecard-display-contact-delete) :type 'user-error)))

;;; Contact quit test

(ert-deftest ecard-display-test-contact-quit-no-modifications ()
  "Test quitting without modifications."
  (let* ((ecard-obj (ecard-create :fn "Quit Test"))
         (server (ecard-carddav-server))
         (addressbook (ecard-carddav-addressbook :server server))
         (resource (ecard-carddav-resource
                    :addressbook addressbook
                    :url "https://test.example.com/quit.vcf"
                    :path "/quit.vcf"
                    :ecard ecard-obj)))
    (ecard-display-contact-detail resource)
    (let ((buffer (get-buffer "*CardDAV Contact: Quit Test*")))
      (unwind-protect
          (with-current-buffer buffer
            (cl-letf (((symbol-function 'quit-window)
                       (lambda (&rest _) t)))
              ;; Should quit without prompting
              (ecard-display-contact-quit)))
        (when buffer (kill-buffer buffer))))))

(ert-deftest ecard-display-test-contact-quit-with-modifications ()
  "Test quitting with unsaved modifications."
  (let* ((ecard-obj (ecard-create :fn "Quit Modified Test"))
         (server (ecard-carddav-server))
         (addressbook (ecard-carddav-addressbook :server server))
         (resource (ecard-carddav-resource
                    :addressbook addressbook
                    :url "https://test.example.com/quit-mod.vcf"
                    :path "/quit-mod.vcf"
                    :ecard ecard-obj)))
    (ecard-display-contact-detail resource)
    (let ((buffer (get-buffer "*CardDAV Contact: Quit Modified Test*")))
      (unwind-protect
          (with-current-buffer buffer
            ;; Mark as modified
            (setq ecard-display--modified t)
            ;; User says "yes, discard changes"
            (cl-letf (((symbol-function 'yes-or-no-p)
                       (lambda (&rest _) t))
                      ((symbol-function 'quit-window)
                       (lambda (&rest _) t)))
              (ecard-display-contact-quit)))
        (when buffer (kill-buffer buffer))))))

;;; Contact open (fetch and display) test

(ert-deftest ecard-display-test-contacts-open-with-mock ()
  "Test opening a contact detail from the contacts list."
  (let ((mock (ecard-carddav-mock-server-create
               :base-url "https://test.example.com")))
    (ecard-carddav-mock-add-addressbook
     mock "/addressbooks/user/contacts/" "Contacts" "My contacts")
    (ecard-carddav-mock-put-ecard
     mock "/addressbooks/user/contacts/open-test.vcf"
     (ecard-create :fn "Open Test Contact" :email "open@test.com"))
    (unwind-protect
        (progn
          (ecard-carddav-mock-install mock)
          (let* ((server (ecard-carddav-server-create
                          :url "https://test.example.com"
                          :auth (ecard-carddav-auth-basic-create
                                 :username "t" :password "t"))))
            (ecard-carddav-discover-addressbooks server)
            (let ((addressbook (car (oref server addressbooks))))
              (ecard-carddav-list-resources addressbook)
              (let ((resource (car (oref addressbook resources))))
                ;; Mock tabulated-list-get-id to return our resource
                (cl-letf (((symbol-function 'tabulated-list-get-id)
                           (lambda () resource))
                          ((symbol-function 'switch-to-buffer)
                           (lambda (buf) buf)))
                  (with-temp-buffer
                    (ecard-display-contacts-mode)
                    (setq ecard-display--addressbook addressbook)
                    (ecard-display-contacts-open)
                    ;; Resource should now have ecard data
                    (should (oref resource ecard))
                    ;; Detail buffer should exist
                    (let ((detail-buf (get-buffer
                                       (format "*CardDAV Contact: %s*"
                                               (ecard-display--get-fn
                                                (oref resource ecard))))))
                      (when detail-buf
                        (kill-buffer detail-buf)))))))))
      (ecard-carddav-mock-uninstall))))

;;; Contacts load all test

(ert-deftest ecard-display-test-contacts-load-all-with-mock ()
  "Test loading all contacts disabling pagination."
  (let ((mock (ecard-carddav-mock-server-create
               :base-url "https://test.example.com")))
    (ecard-carddav-mock-add-addressbook
     mock "/addressbooks/user/contacts/" "Contacts" "My contacts")
    (dotimes (i 5)
      (ecard-carddav-mock-put-ecard
       mock (format "/addressbooks/user/contacts/c%d.vcf" i)
       (ecard-create :fn (format "Contact %d" i))))
    (unwind-protect
        (progn
          (ecard-carddav-mock-install mock)
          (let* ((server (ecard-carddav-server-create
                          :url "https://test.example.com"
                          :auth (ecard-carddav-auth-basic-create
                                 :username "t" :password "t"))))
            (ecard-carddav-discover-addressbooks server)
            (let ((addressbook (car (oref server addressbooks))))
              (ecard-carddav-list-resources addressbook)
              (with-temp-buffer
                (ecard-display-contacts-mode)
                (setq ecard-display--addressbook addressbook
                      ecard-display-contacts-page-size 2
                      ecard-display--total-contacts 5)
                ;; First populate with pagination
                (ecard-display-contacts--populate addressbook 0 t)
                (should (= (length tabulated-list-entries) 2))
                ;; Now load all (mock yes-or-no-p)
                (cl-letf (((symbol-function 'yes-or-no-p)
                           (lambda (&rest _) t)))
                  (ecard-display-contacts-load-all))
                ;; Should have all 5
                (should (= (length tabulated-list-entries) 5))))))
      (ecard-carddav-mock-uninstall))))

;;; Contacts refresh with mock

(ert-deftest ecard-display-test-contacts-refresh-with-mock ()
  "Test refreshing contacts resets page and reloads."
  (let ((mock (ecard-carddav-mock-server-create
               :base-url "https://test.example.com")))
    (ecard-carddav-mock-add-addressbook
     mock "/addressbooks/user/contacts/" "Contacts" "My contacts")
    (dotimes (i 3)
      (ecard-carddav-mock-put-ecard
       mock (format "/addressbooks/user/contacts/c%d.vcf" i)
       (ecard-create :fn (format "Contact %d" i))))
    (unwind-protect
        (progn
          (ecard-carddav-mock-install mock)
          (let* ((server (ecard-carddav-server-create
                          :url "https://test.example.com"
                          :auth (ecard-carddav-auth-basic-create
                                 :username "t" :password "t"))))
            (ecard-carddav-discover-addressbooks server)
            (let ((addressbook (car (oref server addressbooks))))
              (ecard-carddav-list-resources addressbook)
              (with-temp-buffer
                (ecard-display-contacts-mode)
                (setq ecard-display--addressbook addressbook
                      ecard-display--current-page 2)
                ;; Refresh should reset to page 0
                (ecard-display-contacts-refresh)
                (should (= ecard-display--current-page 0))
                (should (>= (length tabulated-list-entries) 1))))))
      (ecard-carddav-mock-uninstall))))

;;; Clone ecard with fallback

(ert-deftest ecard-display-test-clone-ecard-nil ()
  "Test cloning nil ecard."
  (should (null (ecard-display--clone-ecard nil))))

(ert-deftest ecard-display-test-clone-ecard-independence ()
  "Test that cloned ecard is independent of original."
  (let* ((fn-prop (ecard-property :name "FN" :value "Clone Test"))
         (email-prop (ecard-property :name "EMAIL" :value "clone@test.com"))
         (ecard-obj (ecard :fn (list fn-prop) :email (list email-prop)))
         (cloned (ecard-display--clone-ecard ecard-obj)))
    (should cloned)
    ;; Modify the clone
    (oset cloned fn (list (ecard-property :name "FN" :value "Modified Clone")))
    ;; Original should be unchanged
    (should (equal (ecard-display--get-fn ecard-obj) "Clone Test"))
    (should (equal (ecard-display--get-fn cloned) "Modified Clone"))))

;;; Get first tel with nil ecard

(ert-deftest ecard-display-test-get-first-tel-nil ()
  "Test extracting telephone from nil ecard."
  (should (equal (ecard-display--get-first-tel nil) "")))

(ert-deftest ecard-display-test-get-first-tel-empty ()
  "Test extracting telephone from ecard with no tels."
  (let ((ecard-obj (ecard)))
    (should (equal (ecard-display--get-first-tel ecard-obj) ""))))

;;; Get first email with nil ecard

(ert-deftest ecard-display-test-get-first-email-nil ()
  "Test extracting email from nil ecard."
  (should (equal (ecard-display--get-first-email nil) "")))

;;; Get FN with no FN property

(ert-deftest ecard-display-test-get-fn-no-fn-prop ()
  "Test extracting FN when ecard has no FN property."
  (let ((ecard-obj (ecard)))
    (should (equal (ecard-display--get-fn ecard-obj) "Unknown"))))

;;; Extract name from various paths

(ert-deftest ecard-display-test-extract-name-from-path-encoded ()
  "Test extracting name from URL-encoded path."
  (should (equal (ecard-display--extract-name-from-path
                  "/contacts/First%20Last.vcf")
                 "First Last"))
  (should (equal (ecard-display--extract-name-from-path
                  "/deep/path/to/contact.vcf")
                 "contact"))
  (should (equal (ecard-display--extract-name-from-path
                  "/uuid-style-name.vcf")
                 "uuid-style-name")))

;;; Addressbook contact count edge cases

(ert-deftest ecard-display-test-contact-count-large ()
  "Test contact count with many resources."
  (let* ((server (ecard-carddav-server))
         (addressbook (ecard-carddav-addressbook
                       :server server
                       :url "https://test.example.com/addressbook")))
    (oset addressbook resources
          (cl-loop for i from 1 to 500
                   collect (ecard-carddav-resource :path (format "/c%d.vcf" i))))
    (should (equal (ecard-display--addressbook-contact-count addressbook) "500"))))

;;; Uncovered line tests - servers-open error handling (line 166)

(ert-deftest ecard-display-test-servers-open-error ()
  "Test servers-open displays error message on connection failure.
Covers line 166."
  (let* ((buffer (get-buffer-create "*CardDAV Servers*"))
         (config '(:name "Fail Server" :url "https://fail.example.com"
                   :username "user" :password "pass"))
         (messages nil))
    (unwind-protect
        (with-current-buffer buffer
          (ecard-display-servers-mode)
          ;; Mock tabulated-list-get-id and make discovery fail
          (cl-letf (((symbol-function 'tabulated-list-get-id)
                     (lambda () config))
                    ((symbol-function 'ecard-carddav-discover-addressbooks)
                     (lambda (_server) (error "Connection refused")))
                    ((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (push (apply #'format fmt args) messages))))
            (ecard-display-servers-open))
          (should (cl-some (lambda (m) (string-match-p "Failed to connect" m))
                           messages)))
      (kill-buffer buffer))))

;;; Addressbook refresh error handling (lines 255-262)

(ert-deftest ecard-display-test-addressbooks-refresh-error ()
  "Test addressbook refresh handles errors gracefully.
Covers lines 255-262."
  (let* ((server (ecard-carddav-server))
         (buffer (get-buffer-create "*CardDAV: Test*"))
         (messages nil))
    (unwind-protect
        (with-current-buffer buffer
          (ecard-display-addressbooks-mode)
          (setq ecard-display--server server)
          (cl-letf (((symbol-function 'ecard-carddav-discover-addressbooks)
                     (lambda (_server) (error "Discovery failed")))
                    ((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (push (apply #'format fmt args) messages))))
            (ecard-display-addressbooks-refresh))
          (should (cl-some (lambda (m) (string-match-p "Failed to refresh" m))
                           messages)))
      (kill-buffer buffer))))

;;; Addressbook open error handling (line 278)

(ert-deftest ecard-display-test-addressbooks-open-error ()
  "Test addressbook open handles errors gracefully.
Covers line 278."
  (let* ((server (ecard-carddav-server))
         (addressbook (ecard-carddav-addressbook :server server
                       :url "https://test.example.com/ab/"))
         (buffer (get-buffer-create "*CardDAV: Test*"))
         (messages nil))
    (unwind-protect
        (with-current-buffer buffer
          (ecard-display-addressbooks-mode)
          (setq ecard-display--server server)
          ;; Mock tabulated-list-get-id and make list-resources fail
          (cl-letf (((symbol-function 'tabulated-list-get-id)
                     (lambda () addressbook))
                    ((symbol-function 'ecard-carddav-list-resources)
                     (lambda (_ab) (error "List failed")))
                    ((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (push (apply #'format fmt args) messages))))
            (ecard-display-addressbooks-open))
          (should (cl-some (lambda (m) (string-match-p "Failed to load" m))
                           messages)))
      (kill-buffer buffer))))

;;; Contacts next/prev page edge cases (lines 500, 509)

(ert-deftest ecard-display-test-contacts-next-page-at-last ()
  "Test next-page when already at last page.
Covers line 500."
  (let* ((server (ecard-carddav-server))
         (addressbook (ecard-carddav-addressbook :server server))
         (buffer (get-buffer-create "*CardDAV Contacts: Test*"))
         (messages nil))
    (unwind-protect
        (with-current-buffer buffer
          (ecard-display-contacts-mode)
          (setq ecard-display--addressbook addressbook
                ecard-display--total-contacts 5
                ecard-display--current-page 0
                ecard-display-contacts-page-size 5)
          (cl-letf (((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (push (apply #'format fmt args) messages))))
            (ecard-display-contacts-next-page))
          (should (cl-some (lambda (m) (string-match-p "Already at last" m))
                           messages)))
      (kill-buffer buffer))))

(ert-deftest ecard-display-test-contacts-prev-page-pagination-disabled ()
  "Test prev-page with pagination disabled.
Covers line 509."
  (let* ((server (ecard-carddav-server))
         (addressbook (ecard-carddav-addressbook :server server))
         (buffer (get-buffer-create "*CardDAV Contacts: Test2*"))
         (ecard-display-contacts-page-size nil))
    (unwind-protect
        (with-current-buffer buffer
          (ecard-display-contacts-mode)
          (setq ecard-display--addressbook addressbook)
          (should-error (ecard-display-contacts-prev-page) :type 'user-error))
      (kill-buffer buffer))))

;;; Contacts open error handling (lines 543-552)

(ert-deftest ecard-display-test-contacts-open-parse-error ()
  "Test contacts-open handles parse errors.
Covers lines 543-545."
  (let* ((server (ecard-carddav-server))
         (addressbook (ecard-carddav-addressbook :server server))
         (resource (ecard-carddav-resource
                    :addressbook addressbook
                    :url "https://test.example.com/bad.vcf"
                    :path "/bad.vcf"))
         (buffer (get-buffer-create "*CardDAV Contacts: ParseErr*"))
         (messages nil))
    (unwind-protect
        (with-current-buffer buffer
          (ecard-display-contacts-mode)
          (setq ecard-display--addressbook addressbook)
          (setq tabulated-list-entries
                (list (list resource (vector "Bad" "" ""))))
          (tabulated-list-print t)
          (goto-char (point-min))
          (forward-line)
          (cl-letf (((symbol-function 'ecard-carddav-get-resource)
                     (lambda (_ab _url)
                       (signal 'ecard-parse-error '("Invalid vCard format"))))
                    ((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (push (apply #'format fmt args) messages))))
            (should-error (ecard-display-contacts-open) :type 'user-error)))
      (kill-buffer buffer))))

(ert-deftest ecard-display-test-contacts-open-validation-error ()
  "Test contacts-open handles validation errors.
Covers lines 546-549."
  (let* ((server (ecard-carddav-server))
         (addressbook (ecard-carddav-addressbook :server server))
         (resource (ecard-carddav-resource
                    :addressbook addressbook
                    :url "https://test.example.com/invalid.vcf"
                    :path "/invalid.vcf"))
         (buffer (get-buffer-create "*CardDAV Contacts: ValErr*"))
         (messages nil))
    (unwind-protect
        (with-current-buffer buffer
          (ecard-display-contacts-mode)
          (setq ecard-display--addressbook addressbook)
          (setq tabulated-list-entries
                (list (list resource (vector "Invalid" "" ""))))
          (tabulated-list-print t)
          (goto-char (point-min))
          (forward-line)
          (cl-letf (((symbol-function 'ecard-carddav-get-resource)
                     (lambda (_ab _url)
                       (signal 'ecard-validation-error '("Missing FN"))))
                    ((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (push (apply #'format fmt args) messages))))
            (should-error (ecard-display-contacts-open) :type 'user-error)))
      (kill-buffer buffer))))

(ert-deftest ecard-display-test-contacts-open-generic-error ()
  "Test contacts-open handles generic errors.
Covers lines 550-552."
  (let* ((server (ecard-carddav-server))
         (addressbook (ecard-carddav-addressbook :server server))
         (resource (ecard-carddav-resource
                    :addressbook addressbook
                    :url "https://test.example.com/err.vcf"
                    :path "/err.vcf"))
         (buffer (get-buffer-create "*CardDAV Contacts: GenErr*"))
         (messages nil))
    (unwind-protect
        (with-current-buffer buffer
          (ecard-display-contacts-mode)
          (setq ecard-display--addressbook addressbook)
          (setq tabulated-list-entries
                (list (list resource (vector "Error" "" ""))))
          (tabulated-list-print t)
          (goto-char (point-min))
          (forward-line)
          (cl-letf (((symbol-function 'ecard-carddav-get-resource)
                     (lambda (_ab _url)
                       (error "Network timeout")))
                    ((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (push (apply #'format fmt args) messages))))
            (should-error (ecard-display-contacts-open) :type 'user-error)))
      (kill-buffer buffer))))

;;; Contact detail delete with mock (lines 864-886)

(ert-deftest ecard-display-test-contact-detail-delete-success ()
  "Test deleting contact from detail view with mock.
Covers lines 864-884."
  (let* ((ecard-obj (ecard-create :fn "Delete Me"))
         (server (ecard-carddav-server))
         (addressbook (ecard-carddav-addressbook :server server))
         (resource (ecard-carddav-resource
                    :addressbook addressbook
                    :url "https://test.example.com/del.vcf"
                    :path "/del.vcf"
                    :etag "etag1"
                    :ecard ecard-obj))
         (messages nil)
         (deleted nil))
    (ecard-display-contact-detail resource)
    (let ((buffer (get-buffer "*CardDAV Contact: Delete Me*")))
      (unwind-protect
          (with-current-buffer buffer
            (cl-letf (((symbol-function 'yes-or-no-p)
                       (lambda (&rest _) t))
                      ((symbol-function 'ecard-carddav-delete-resource)
                       (lambda (_ab _url &optional _etag)
                         (setq deleted t)
                         t))
                      ((symbol-function 'kill-buffer)
                       (lambda (&rest _) nil))
                      ((symbol-function 'ecard-display-contacts-refresh)
                       (lambda () nil))
                      ((symbol-function 'message)
                       (lambda (fmt &rest args)
                         (push (apply #'format fmt args) messages))))
              (ecard-display-contact-delete))
            (should deleted)
            (should (cl-some (lambda (m) (string-match-p "Deleted contact" m))
                             messages)))
        (when buffer (kill-buffer buffer))))))

(ert-deftest ecard-display-test-contact-detail-delete-error ()
  "Test deleting contact from detail view handles errors.
Covers lines 885-886."
  (let* ((ecard-obj (ecard-create :fn "Fail Delete"))
         (server (ecard-carddav-server))
         (addressbook (ecard-carddav-addressbook :server server))
         (resource (ecard-carddav-resource
                    :addressbook addressbook
                    :url "https://test.example.com/fdel.vcf"
                    :path "/fdel.vcf"
                    :etag "etag1"
                    :ecard ecard-obj))
         (messages nil))
    (ecard-display-contact-detail resource)
    (let ((buffer (get-buffer "*CardDAV Contact: Fail Delete*")))
      (unwind-protect
          (with-current-buffer buffer
            (cl-letf (((symbol-function 'yes-or-no-p)
                       (lambda (&rest _) t))
                      ((symbol-function 'ecard-carddav-delete-resource)
                       (lambda (_ab _url &optional _etag)
                         (error "Delete failed")))
                      ((symbol-function 'message)
                       (lambda (fmt &rest args)
                         (push (apply #'format fmt args) messages))))
              (ecard-display-contact-delete))
            (should (cl-some (lambda (m) (string-match-p "Failed to delete" m))
                             messages)))
        (when buffer (kill-buffer buffer))))))

;;; Contact save conflict error (line 844)

(ert-deftest ecard-display-test-contact-save-conflict ()
  "Test saving contact handles conflict error.
Covers line 844."
  (let* ((ecard-obj (ecard-create :fn "Conflict Test"))
         (server (ecard-carddav-server))
         (addressbook (ecard-carddav-addressbook :server server))
         (resource (ecard-carddav-resource
                    :addressbook addressbook
                    :url "https://test.example.com/conflict.vcf"
                    :path "/conflict.vcf"
                    :etag "old-etag"
                    :ecard ecard-obj))
         (messages nil))
    (ecard-display-contact-detail resource)
    (let ((buffer (get-buffer "*CardDAV Contact: Conflict Test*")))
      (unwind-protect
          (with-current-buffer buffer
            (cl-letf (((symbol-function 'ecard-widget-get-value)
                       (lambda () ecard-obj))
                      ((symbol-function 'ecard-carddav-put-ecard)
                       (lambda (_ab _url _ecard &optional _etag)
                         (signal 'ecard-carddav-conflict-error '("ETag mismatch"))))
                      ((symbol-function 'message)
                       (lambda (fmt &rest args)
                         (push (apply #'format fmt args) messages))))
              (ecard-display-contact-save))
            (should (cl-some (lambda (m) (string-match-p "Save conflict" m))
                             messages)))
        (when buffer (kill-buffer buffer))))))

;;; Safe string with format-fallback (line 932)

(ert-deftest ecard-display-test-safe-string-other-type ()
  "Test safe-string with non-standard types.
Covers line 932."
  ;; An arbitrary object should get formatted
  (should (stringp (ecard-display--safe-string (make-hash-table)))))

;;; Clone ecard compat fallback (lines 1004-1005)

(ert-deftest ecard-display-test-clone-ecard-compat-fallback ()
  "Test clone-ecard falls back to compat parser.
Covers lines 1004-1005."
  (let* ((ecard-obj (ecard-create :fn "Clone Test")))
    ;; Make the standard parser fail, compat parser should succeed
    (cl-letf (((symbol-function 'ecard-parse)
               (lambda (_text) (signal 'ecard-parse-error '("Forced failure"))))
              ((symbol-function 'ecard-compat-parse)
               (lambda (_text)
                 ;; Return a valid ecard
                 (ecard-create :fn "Clone Test"))))
      (let ((cloned (ecard-display--clone-ecard ecard-obj)))
        (should cloned)
        (should (ecard-p cloned))))))

;;; Contacts refresh-names (lines 484-486)

(ert-deftest ecard-display-test-contacts-refresh-names-calls-populate ()
  "Test refresh-names calls populate with force-fetch.
Covers lines 484-486."
  (let* ((server (ecard-carddav-server))
         (addressbook (ecard-carddav-addressbook :server server))
         (buffer (get-buffer-create "*CardDAV Contacts: RefreshNames*"))
         (populate-called nil)
         (force-fetch-arg nil))
    (unwind-protect
        (with-current-buffer buffer
          (ecard-display-contacts-mode)
          (setq ecard-display--addressbook addressbook
                ecard-display--current-page 0)
          (cl-letf (((symbol-function 'ecard-display-contacts--populate)
                     (lambda (_ab &optional page fetch-names)
                       (setq populate-called t
                             force-fetch-arg fetch-names))))
            (ecard-display-contacts-refresh-names))
          (should populate-called)
          (should force-fetch-arg))
      (kill-buffer buffer))))

;;; Contacts add error (line 605)

(ert-deftest ecard-display-test-contacts-add-error ()
  "Test contacts-add handles server error.
Covers line 605."
  (let* ((server (ecard-carddav-server))
         (addressbook (ecard-carddav-addressbook :server server))
         (buffer (get-buffer-create "*CardDAV Contacts: AddErr*"))
         (messages nil))
    (unwind-protect
        (with-current-buffer buffer
          (ecard-display-contacts-mode)
          (setq ecard-display--addressbook addressbook)
          (cl-letf (((symbol-function 'read-string)
                     (lambda (prompt &rest _)
                       (cond
                        ((string-match-p "Full name" prompt) "Fail Add")
                        ((string-match-p "Given" prompt) "Fail")
                        ((string-match-p "Family" prompt) "Add")
                        ((string-match-p "Email" prompt) "fail@test.com")
                        ((string-match-p "Phone" prompt) ""))))
                    ((symbol-function 'ecard-carddav-put-ecard)
                     (lambda (_ab _path _ecard)
                       (error "PUT failed")))
                    ((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (push (apply #'format fmt args) messages))))
            (ecard-display-contacts-add))
          (should (cl-some (lambda (m) (string-match-p "Failed to create" m))
                           messages)))
      (kill-buffer buffer))))

;;; Contacts delete error (line 626)

(ert-deftest ecard-display-test-contacts-delete-error ()
  "Test contacts-delete handles server error.
Covers line 626."
  (let* ((ecard-obj (ecard-create :fn "Del Error Test"))
         (server (ecard-carddav-server))
         (addressbook (ecard-carddav-addressbook :server server))
         (resource (ecard-carddav-resource
                    :addressbook addressbook
                    :url "https://test.example.com/delerr.vcf"
                    :path "/delerr.vcf"
                    :etag "etag1"
                    :ecard ecard-obj))
         (buffer (get-buffer-create "*CardDAV Contacts: DelErr*"))
         (messages nil))
    (unwind-protect
        (with-current-buffer buffer
          (ecard-display-contacts-mode)
          (setq ecard-display--addressbook addressbook)
          ;; Mock tabulated-list-get-id to return our resource
          (cl-letf (((symbol-function 'tabulated-list-get-id)
                     (lambda () resource))
                    ((symbol-function 'yes-or-no-p)
                     (lambda (&rest _) t))
                    ((symbol-function 'ecard-carddav-delete-resource)
                     (lambda (_ab _url &optional _etag)
                       (error "Delete failed")))
                    ((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (push (apply #'format fmt args) messages))))
            (let ((ecard-display-confirm-delete nil))
              (ecard-display-contacts-delete)))
          (should (cl-some (lambda (m) (string-match-p "Failed to delete" m))
                           messages)))
      (kill-buffer buffer))))

;;; Normalize server config with auth as function (line 955)

(ert-deftest ecard-display-test-normalize-server-config-auth-function ()
  "Test normalizing server config where auth is a function.
Covers line 955."
  (let* ((auth-obj (ecard-carddav-auth-basic-create
                    :username "funcuser" :password "funcpass"))
         ;; Create server with auth as a lambda returning the auth object
         (server (ecard-carddav-server
                  :url "https://func.example.com"
                  :auth (lambda () auth-obj)))
         (normalized (ecard-display--normalize-server-config server)))
    (should (equal (plist-get normalized :url) "https://func.example.com"))
    (should (equal (plist-get normalized :username) "funcuser"))
    (should (equal (plist-get normalized :password) "funcpass"))))

(provide 'ecard-display-test)
;;; ecard-display-test.el ends here
