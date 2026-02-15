;;; ecard-sync-test.el --- Tests for ecard-sync -*- lexical-binding: t; -*-

;; Copyright (C) 2025 John Wiegley

;; Author: John Wiegley <johnw@gnu.org>
;; Version: 1.0.0

;;; Commentary:

;; Unit tests for ecard-sync.el org-contacts synchronization.

;;; Code:

(require 'ert)
(require 'ecard)
(require 'ecard-sync)

;;; Test utilities

(defun ecard-sync-test--create-org-buffer ()
  "Create temporary org-mode buffer with test contact."
  (with-current-buffer (generate-new-buffer "*ecard-sync-test*")
    (org-mode)
    (insert "* John Doe                                                    :Council:Test:\n")
    (insert ":PROPERTIES:\n")
    (insert ":ID:       550e8400-e29b-41d4-a716-446655440000\n")
    (insert ":CREATED:  [2025-01-28 Tue 12:00]\n")
    (insert ":EMAIL:    john.doe@example.com\n")
    (insert ":EMAIL2:   johnd@work.example.com\n")
    (insert ":PHONE:    +1-555-1234\n")
    (insert ":ADDRESS:  ;;123 Main St;Suite 100;Springfield;IL;62701;USA\n")
    (insert ":ORG:      Example Corp\n")
    (insert ":BIRTHDAY: 1990-05-15\n")
    (insert ":LOCATION: 39.781721,-89.650148\n")
    (insert ":END:\n")
    (insert "Test contact\n")
    (goto-char (point-min))
    (re-search-forward "^\\*")
    (beginning-of-line)
    (current-buffer)))

;;; Name splitting tests

(ert-deftest ecard-sync-test-split-name-two-parts ()
  "Test splitting two-part names."
  (let ((result (ecard-sync--split-name "John Doe")))
    (should (equal result '("Doe" "John" "" "" "")))))

(ert-deftest ecard-sync-test-split-name-three-parts ()
  "Test splitting three-part names."
  (let ((result (ecard-sync--split-name "John Michael Doe")))
    (should (equal result '("Doe" "John" "Michael" "" "")))))

(ert-deftest ecard-sync-test-split-name-single ()
  "Test splitting single names."
  (let ((result (ecard-sync--split-name "Madonna")))
    (should (equal result '("" "Madonna" "" "" "")))))

;;; Location parsing tests

(ert-deftest ecard-sync-test-parse-location ()
  "Test parsing location string to geo URI."
  (should (equal (ecard-sync--parse-location "39.781721,-89.650148")
                 "geo:39.781721,-89.650148"))
  (should (null (ecard-sync--parse-location "invalid"))))

(ert-deftest ecard-sync-test-format-location ()
  "Test formatting geo URI to location string."
  (should (equal (ecard-sync--format-location "geo:39.781721,-89.650148")
                 "39.781721,-89.650148"))
  (should (null (ecard-sync--format-location "invalid"))))

;;; Org-to-ecard conversion tests

(ert-deftest ecard-sync-test-org-to-ecard-basic ()
  "Test basic org-to-ecard conversion."
  (with-current-buffer (ecard-sync-test--create-org-buffer)
    (let ((vc (ecard-sync-org-to-ecard)))
      ;; Check FN
      (should (equal (ecard-get-property-value vc 'fn) "John Doe"))

      ;; Check N
      (let ((n (ecard-get-property-value vc 'n)))
        (should (equal n '("Doe" "John" "" "" ""))))

      ;; Check emails
      (let ((emails (ecard-get-property-values vc 'email)))
        (should (equal (length emails) 2))
        (should (equal (car emails) "john.doe@example.com"))
        (should (equal (cadr emails) "johnd@work.example.com")))

      ;; Check phone
      (should (equal (ecard-get-property-value vc 'tel) "+1-555-1234"))

      ;; Check address
      (let ((adr (ecard-get-property-value vc 'adr)))
        (should (listp adr))
        (should (equal (nth 2 adr) "123 Main St")))

      ;; Check organization
      (let ((org (ecard-get-property-value vc 'org)))
        (should (equal org '("Example Corp"))))

      ;; Check note
      (should (equal (ecard-get-property-value vc 'note) "Test contact"))

      ;; Check birthday
      (should (equal (ecard-get-property-value vc 'bday) "1990-05-15"))

      ;; Check geo
      (should (equal (ecard-get-property-value vc 'geo)
                     "geo:39.781721,-89.650148"))

      ;; Check categories (tags)
      (let ((cats (ecard-get-property-value vc 'categories)))
        (should (member "Council" cats))
        (should (member "Test" cats)))

      ;; Check UID matches org ID
      (should (equal (ecard-get-property-value vc 'uid)
                     "550e8400-e29b-41d4-a716-446655440000")))

    (kill-buffer)))

(ert-deftest ecard-sync-test-org-to-ecard-minimal ()
  "Test org-to-ecard with minimal properties."
  (with-current-buffer (generate-new-buffer "*ecard-sync-test-minimal*")
    (org-mode)
    (insert "* Jane Smith\n")
    (insert ":PROPERTIES:\n")
    (insert ":ID:       123e4567-e89b-12d3-a456-426614174000\n")
    (insert ":END:\n")
    (goto-char (point-min))
    (re-search-forward "^\\*")
    (beginning-of-line)

    (let ((vc (ecard-sync-org-to-ecard)))
      ;; Should have FN and N
      (should (equal (ecard-get-property-value vc 'fn) "Jane Smith"))
      (should (equal (ecard-get-property-value vc 'n) '("Smith" "Jane" "" "" "")))

      ;; Should have VERSION
      (should (equal (ecard-get-property-value vc 'version) "4.0"))

      ;; Should have UID from org ID
      (should (equal (ecard-get-property-value vc 'uid)
                     "123e4567-e89b-12d3-a456-426614174000")))

    (kill-buffer)))

;;; Ecard-to-org update tests

(ert-deftest ecard-sync-test-ecard-to-org ()
  "Test ecard-to-org update."
  (with-current-buffer (ecard-sync-test--create-org-buffer)
    ;; Create ecard with new values
    (let ((vc (ecard-create
               :fn "John Doe"
               :n '("Doe" "John" "" "" "")
               :email '("newemail@example.com" "newemail2@example.com")
               :tel "+1-555-9999"
               :org "New Corp"
               :uid "test-uid-12345")))

      ;; Update org entry without changing ID
      (ecard-sync-ecard-to-org vc)

      ;; Check updated properties
      (should (equal (org-entry-get (point) "EMAIL") "newemail@example.com"))
      (should (equal (org-entry-get (point) "EMAIL2") "newemail2@example.com"))
      (should (equal (org-entry-get (point) "PHONE") "+1-555-9999"))
      (should (equal (org-entry-get (point) "ORG") "New Corp"))

      ;; ID should be preserved (update-id was nil)
      (should (equal (org-entry-get (point) "ID")
                    "550e8400-e29b-41d4-a716-446655440000")))

    (kill-buffer)))

(ert-deftest ecard-sync-test-ecard-to-org-update-id ()
  "Test ecard-to-org with ID update (adopting server UID)."
  (with-current-buffer (ecard-sync-test--create-org-buffer)
    ;; Create ecard with server's UID
    (let ((vc (ecard-create
               :fn "John Doe"
               :uid "server-uid-99999")))

      ;; Update org entry WITH ID update (simulating adoption of server UID)
      (ecard-sync-ecard-to-org vc t)

      ;; ID should be changed to server's UID
      (should (equal (org-entry-get (point) "ID") "server-uid-99999")))

    (kill-buffer)))

;;; Merge tests

(ert-deftest ecard-sync-test-merge-prefer-local ()
  "Test merging with local preference.
Local is authoritative: local values win, and server-only
values are discarded (local deletions propagate)."
  (let ((ecard-sync-prefer-local t)
        (local-vc (ecard-create
                   :fn "John Doe"
                   :email "local@example.com"
                   :tel "+1-555-1111"))
        (server-vc (ecard-create
                    :fn "John Doe"
                    :email "server@example.com"
                    :tel "+1-555-2222"
                    :note "Server note")))

    (let ((merged (ecard-sync--merge-ecards local-vc server-vc)))
      ;; Should prefer local email and tel
      (should (equal (ecard-get-property-value merged 'email) "local@example.com"))
      (should (equal (ecard-get-property-value merged 'tel) "+1-555-1111"))

      ;; Server-only note should be discarded (local is authoritative)
      (should (null (ecard-get-property-value merged 'note))))))

(ert-deftest ecard-sync-test-merge-prefer-server ()
  "Test merging with server preference."
  (let ((ecard-sync-prefer-local nil)
        (local-vc (ecard-create
                   :fn "John Doe"
                   :email "local@example.com"
                   :tel "+1-555-1111"))
        (server-vc (ecard-create
                    :fn "John Doe"
                    :email "server@example.com"
                    :tel "+1-555-2222")))

    (let ((merged (ecard-sync--merge-ecards local-vc server-vc)))
      ;; Should prefer server email and tel
      (should (equal (ecard-get-property-value merged 'email) "server@example.com"))
      (should (equal (ecard-get-property-value merged 'tel) "+1-555-2222")))))

;;; Timestamp parsing tests

(ert-deftest ecard-sync-test-parse-rev-timestamp ()
  "Test parsing vCard REV timestamps."
  (let ((time (ecard-sync--parse-rev-timestamp "20250128T120000Z")))
    (should time)
    (should (equal (format-time-string "%Y-%m-%d %H:%M:%S" time t)
                   "2025-01-28 12:00:00")))
  ;; Invalid formats return nil
  (should (null (ecard-sync--parse-rev-timestamp nil)))
  (should (null (ecard-sync--parse-rev-timestamp "")))
  (should (null (ecard-sync--parse-rev-timestamp "2025-01-28T12:00:00Z")))
  (should (null (ecard-sync--parse-rev-timestamp "not-a-date"))))

(ert-deftest ecard-sync-test-parse-org-timestamp ()
  "Test parsing Org-mode timestamps."
  (let ((time (ecard-sync--parse-org-timestamp "[2025-01-28 Tue 12:00]")))
    (should time)
    (should (equal (format-time-string "%Y-%m-%d %H:%M" time)
                   "2025-01-28 12:00")))
  ;; Invalid inputs return nil
  (should (null (ecard-sync--parse-org-timestamp nil)))
  (should (null (ecard-sync--parse-org-timestamp ""))))

;;; Merge direction tests

(ert-deftest ecard-sync-test-determine-prefer-local-no-synced ()
  "Test merge direction when SYNCED is absent.
Falls back to `ecard-sync-prefer-local'."
  (let ((ecard-sync-prefer-local t))
    (should (eq (ecard-sync--determine-prefer-local nil "20250128T120000Z") t)))
  (let ((ecard-sync-prefer-local nil))
    (should (eq (ecard-sync--determine-prefer-local nil "20250128T120000Z") nil))))

(ert-deftest ecard-sync-test-determine-prefer-local-server-newer ()
  "Test merge direction when server REV is newer than SYNCED.
Should prefer server (return nil)."
  (let ((ecard-sync-prefer-local t))
    ;; SYNCED: Jan 28 12:00, REV: Jan 29 12:00 → server newer → nil
    (should (eq (ecard-sync--determine-prefer-local
                 "[2025-01-28 Tue 12:00]" "20250129T120000Z")
                nil))))

(ert-deftest ecard-sync-test-determine-prefer-local-server-older ()
  "Test merge direction when server REV is older than SYNCED.
Should prefer local (return t)."
  (let ((ecard-sync-prefer-local nil))
    ;; SYNCED: Jan 29 12:00, REV: Jan 28 12:00 → server older → t
    (should (eq (ecard-sync--determine-prefer-local
                 "[2025-01-29 Wed 12:00]" "20250128T120000Z")
                t))))

(ert-deftest ecard-sync-test-determine-prefer-local-no-rev ()
  "Test merge direction when server has no REV.
Falls back to `ecard-sync-prefer-local'."
  (let ((ecard-sync-prefer-local t))
    (should (eq (ecard-sync--determine-prefer-local
                 "[2025-01-28 Tue 12:00]" nil)
                t)))
  (let ((ecard-sync-prefer-local nil))
    (should (eq (ecard-sync--determine-prefer-local
                 "[2025-01-28 Tue 12:00]" nil)
                nil))))

;;; Round-trip test

(ert-deftest ecard-sync-test-round-trip ()
  "Test round-trip org-to-ecard-to-org conversion."
  (with-current-buffer (ecard-sync-test--create-org-buffer)
    (let* ((vc1 (ecard-sync-org-to-ecard))
           (fn1 (ecard-get-property-value vc1 'fn))
           (email1 (ecard-get-property-value vc1 'email)))

      ;; Update org from ecard
      (ecard-sync-ecard-to-org vc1)

      ;; Convert back to ecard
      (let* ((vc2 (ecard-sync-org-to-ecard))
             (fn2 (ecard-get-property-value vc2 'fn))
             (email2 (ecard-get-property-value vc2 'email)))

        ;; Values should match
        (should (equal fn1 fn2))
        (should (equal email1 email2))))

    (kill-buffer)))

;;; Multibyte request advice tests

(ert-deftest ecard-sync-test-fix-multibyte-request-utf8 ()
  "Test that the multibyte request advice preserves UTF-8 body bytes.
Simulates the scenario where headers are multibyte ASCII and body
is unibyte UTF-8 containing bytes >= 0x80 (e.g., Bahá'í)."
  (let* ((test-body (encode-coding-string "ORG:Bahá'í National Center\r\n" 'utf-8))
         ;; Simulate multibyte ASCII headers (as url-http-create-request produces)
         (test-headers (concat "PUT /test HTTP/1.1\r\n"
                               "Host: example.com\r\n"
                               "\r\n"))
         ;; Build request the same way the advice does
         (header-bytes
          (encode-coding-string
           (concat (substring test-headers 0 (- (length test-headers) 2))
                   (format "Content-length: %d\r\n" (string-bytes test-body))
                   "\r\n")
           'utf-8))
         (request (concat header-bytes test-body)))
    ;; Result must be unibyte (string-bytes = length)
    (should (not (multibyte-string-p request)))
    (should (= (string-bytes request) (length request)))
    ;; Body bytes must be preserved exactly
    (let ((body-start (string-match "\\(\n\\)\n" (decode-coding-string request 'utf-8))))
      (should body-start)
      ;; The UTF-8 bytes for "á" (0xC3 0xA1) and "í" (0xC3 0xAD) must be intact
      (should (string-match-p "Bah" (decode-coding-string request 'utf-8))))))

;;; Debug logging tests

(ert-deftest ecard-sync-test-debug-enabled ()
  "Test debug logging when enabled."
  (let ((ecard-sync--debug t)
        (messages nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args)
                 (push (apply #'format fmt args) messages))))
      (ecard-sync--debug "test %s %d" "foo" 42))
    (should (= (length messages) 1))
    (should (string-match-p "\\[ecard-sync\\] test foo 42" (car messages)))))

(ert-deftest ecard-sync-test-debug-disabled ()
  "Test debug logging when disabled."
  (let ((ecard-sync--debug nil)
        (messages nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args)
                 (push (apply #'format fmt args) messages))))
      (ecard-sync--debug "should not appear"))
    (should (null messages))))

;;; Cache clearing tests

(ert-deftest ecard-sync-test-clear-cache ()
  "Test cache clearing."
  (let ((ecard-sync--addressbook-cache 'some-value)
        (ecard-sync--resource-cache 'some-resources))
    (ecard-sync-clear-cache)
    (should (null ecard-sync--addressbook-cache))
    (should (null ecard-sync--resource-cache))))

;;; Get server tests

(ert-deftest ecard-sync-test-get-server-not-configured ()
  "Test get-server when not configured."
  (let ((ecard-sync-server nil))
    (should-error (ecard-sync--get-server) :type 'user-error)))

(ert-deftest ecard-sync-test-get-server-with-server-object ()
  "Test get-server with already-configured server object."
  (let* ((auth (ecard-carddav-auth-basic-create
                :username "test" :password "pass"))
         (server (ecard-carddav-server-create :url "https://example.com" :auth auth))
         (ecard-sync-server server))
    (should (eq (ecard-sync--get-server) server))))

(ert-deftest ecard-sync-test-get-server-with-plist ()
  "Test get-server with plist configuration."
  (let ((ecard-sync-server '(:url "https://example.com"
                             :username "user"
                             :password "pass")))
    (let ((result (ecard-sync--get-server)))
      (should (ecard-carddav-server-p result))
      (should (equal (oref result url) "https://example.com")))))

;;; Empty value tests

(ert-deftest ecard-sync-test-empty-value-p ()
  "Test empty value detection."
  ;; nil is empty
  (should (ecard-sync--empty-value-p nil))
  ;; Empty string is empty
  (should (ecard-sync--empty-value-p ""))
  ;; Whitespace-only string is empty
  (should (ecard-sync--empty-value-p "   "))
  (should (ecard-sync--empty-value-p "\t\n"))
  ;; Non-empty string is not empty
  (should-not (ecard-sync--empty-value-p "hello"))
  ;; List of all empty/nil elements is empty
  (should (ecard-sync--empty-value-p '(nil "" "  ")))
  ;; List with at least one non-empty element is not empty
  (should-not (ecard-sync--empty-value-p '("" "hello" "")))
  ;; Non-string, non-list, non-nil is not empty
  (should-not (ecard-sync--empty-value-p 42)))

(ert-deftest ecard-sync-test-slot-empty-p ()
  "Test slot empty detection with ecard-property lists."
  ;; nil list is empty
  (should (ecard-sync--slot-empty-p nil))
  ;; List with empty-valued properties is empty
  (should (ecard-sync--slot-empty-p
           (list (ecard-property :name "TEL" :value ""))))
  (should (ecard-sync--slot-empty-p
           (list (ecard-property :name "TEL" :value "  "))))
  ;; List with non-empty property is not empty
  (should-not (ecard-sync--slot-empty-p
               (list (ecard-property :name "TEL" :value "+1-555-1234"))))
  ;; Mixed: one empty, one non-empty
  (should-not (ecard-sync--slot-empty-p
               (list (ecard-property :name "TEL" :value "")
                     (ecard-property :name "TEL" :value "+1-555-5678")))))

;;; Clean string tests

(ert-deftest ecard-sync-test-clean-string ()
  "Test invisible Unicode character stripping."
  ;; Normal string unchanged
  (should (equal (ecard-sync--clean-string "Hello World") "Hello World"))
  ;; Nil returns nil
  (should (null (ecard-sync--clean-string nil)))
  ;; Zero-width space removed
  (should (equal (ecard-sync--clean-string "Hello\x200bWorld") "HelloWorld"))
  ;; Soft hyphen removed
  (should (equal (ecard-sync--clean-string "Hello\xadWorld") "HelloWorld")))

;;; Location parsing edge cases

(ert-deftest ecard-sync-test-parse-location-nil ()
  "Test parse-location with nil input."
  (should (null (ecard-sync--parse-location nil))))

(ert-deftest ecard-sync-test-parse-location-negative ()
  "Test parse-location with negative coordinates."
  (should (equal (ecard-sync--parse-location "-33.8688,151.2093")
                 "geo:-33.8688,151.2093")))

(ert-deftest ecard-sync-test-format-location-nil ()
  "Test format-location with nil input."
  (should (null (ecard-sync--format-location nil))))

;;; Multibyte request advice tests

(ert-deftest ecard-sync-test-fix-multibyte-request-no-body ()
  "Test multibyte request fix when no body data is present."
  (require 'url-http)
  (let ((orig-data (and (boundp 'url-http-data) url-http-data)))
    (unwind-protect
        (let ((orig-called nil))
          (setq url-http-data nil)
          (let ((orig-fn (lambda ()
                           (setq orig-called t)
                           "GET /test HTTP/1.1\r\nHost: example.com\r\n\r\n")))
            (ecard-sync--fix-multibyte-request orig-fn)
            ;; With no body, original function should be called directly
            (should orig-called)))
      (setq url-http-data orig-data))))

(ert-deftest ecard-sync-test-fix-multibyte-request-empty-body ()
  "Test multibyte request fix when body is empty string."
  (require 'url-http)
  (let ((orig-data (and (boundp 'url-http-data) url-http-data)))
    (unwind-protect
        (let ((orig-called nil))
          (setq url-http-data "")
          (let ((orig-fn (lambda ()
                           (setq orig-called t)
                           "GET /test HTTP/1.1\r\nHost: example.com\r\n\r\n")))
            (ecard-sync--fix-multibyte-request orig-fn)
            ;; Empty body should call original directly
            (should orig-called)))
      (setq url-http-data orig-data))))

(ert-deftest ecard-sync-test-fix-multibyte-request-ascii-body ()
  "Test multibyte request fix with ASCII-only body."
  (require 'url-http)
  (let ((orig-data (and (boundp 'url-http-data) url-http-data)))
    (unwind-protect
        (progn
          (setq url-http-data "FN:John Doe\r\n")
          (let* ((orig-fn (lambda ()
                            "PUT /test HTTP/1.1\r\nHost: example.com\r\n\r\n"))
                 (result (ecard-sync--fix-multibyte-request orig-fn)))
            ;; Result should be unibyte
            (should (not (multibyte-string-p result)))
            ;; Result should contain Content-length header
            (should (string-match-p "Content-length:"
                                    (decode-coding-string result 'utf-8)))
            ;; Result should contain body
            (should (string-match-p "FN:John Doe"
                                    (decode-coding-string result 'utf-8)))))
      (setq url-http-data orig-data))))

;;; Org-to-ecard edge cases

(ert-deftest ecard-sync-test-org-to-ecard-no-org-mode ()
  "Test org-to-ecard fails outside org-mode."
  (with-temp-buffer
    (should-error (ecard-sync-org-to-ecard) :type 'user-error)))

(ert-deftest ecard-sync-test-org-to-ecard-address-with-semicolons ()
  "Test org-to-ecard with structured address (semicolons)."
  (with-current-buffer (generate-new-buffer "*ecard-sync-test-adr*")
    (org-mode)
    (insert "* Test Person\n")
    (insert ":PROPERTIES:\n")
    (insert ":ADDRESS:  ;Suite 100;123 Main St;Springfield;IL;62701;USA\n")
    (insert ":END:\n")
    (goto-char (point-min))
    (re-search-forward "^\\*")
    (beginning-of-line)
    (let ((vc (ecard-sync-org-to-ecard)))
      (let ((adr (ecard-get-property-value vc 'adr)))
        (should (listp adr))
        (should (equal (nth 2 adr) "123 Main St"))
        (should (equal (nth 4 adr) "IL"))))
    (kill-buffer)))

(ert-deftest ecard-sync-test-org-to-ecard-plain-address ()
  "Test org-to-ecard with plain (non-structured) address."
  (with-current-buffer (generate-new-buffer "*ecard-sync-test-plain-adr*")
    (org-mode)
    (insert "* Test Person\n")
    (insert ":PROPERTIES:\n")
    (insert ":ADDRESS:  123 Main St, Springfield, IL\n")
    (insert ":END:\n")
    (goto-char (point-min))
    (re-search-forward "^\\*")
    (beginning-of-line)
    (let ((vc (ecard-sync-org-to-ecard)))
      (let ((adr (ecard-get-property-value vc 'adr)))
        (should (listp adr))
        ;; Plain address goes into street field (index 2)
        (should (equal (nth 2 adr) "123 Main St, Springfield, IL"))))
    (kill-buffer)))

;;; Ecard-to-org edge cases

(ert-deftest ecard-sync-test-ecard-to-org-not-org-mode ()
  "Test ecard-to-org fails outside org-mode."
  (with-temp-buffer
    (should-error (ecard-sync-ecard-to-org (ecard-create :fn "Test"))
                  :type 'user-error)))

(ert-deftest ecard-sync-test-ecard-to-org-not-ecard ()
  "Test ecard-to-org fails with non-ecard argument."
  (with-current-buffer (generate-new-buffer "*ecard-sync-test-bad-arg*")
    (org-mode)
    (insert "* Test\n")
    (goto-char (point-min))
    (should-error (ecard-sync-ecard-to-org "not-an-ecard") :type 'user-error)
    (kill-buffer)))

(ert-deftest ecard-sync-test-ecard-to-org-with-geo ()
  "Test ecard-to-org with GEO property."
  (with-current-buffer (ecard-sync-test--create-org-buffer)
    (let ((vc (ecard-create :fn "John Doe"
                            :geo "geo:37.386,-122.082")))
      (ecard-sync-ecard-to-org vc)
      (should (equal (org-entry-get (point) "LOCATION") "37.386,-122.082")))
    (kill-buffer)))

(ert-deftest ecard-sync-test-ecard-to-org-with-categories ()
  "Test ecard-to-org with categories (org tags)."
  (with-current-buffer (generate-new-buffer "*ecard-sync-test-cats*")
    (org-mode)
    (insert "* Test Person\n")
    (insert ":PROPERTIES:\n")
    (insert ":END:\n")
    (goto-char (point-min))
    (re-search-forward "^\\*")
    (beginning-of-line)
    (let ((vc (ecard-create :fn "Test Person")))
      ;; Set categories as a single property with list value,
      ;; matching how ecard-sync-org-to-ecard creates them
      (ecard-set-property vc 'categories '("Work" "VIP"))
      (ecard-sync-ecard-to-org vc)
      (org-back-to-heading t)
      (let ((tags (org-get-tags nil t)))
        (should (member "Work" tags))
        (should (member "VIP" tags))))
    (kill-buffer)))

(ert-deftest ecard-sync-test-ecard-to-org-with-note ()
  "Test ecard-to-org with note (body text)."
  (with-current-buffer (ecard-sync-test--create-org-buffer)
    (let ((vc (ecard-create :fn "John Doe"
                            :note "Updated note text")))
      (ecard-sync-ecard-to-org vc)
      ;; Convert back and check note
      (let ((vc2 (ecard-sync-org-to-ecard)))
        (should (equal (ecard-get-property-value vc2 'note) "Updated note text"))))
    (kill-buffer)))

(ert-deftest ecard-sync-test-ecard-to-org-address-list ()
  "Test ecard-to-org with address as list."
  (with-current-buffer (ecard-sync-test--create-org-buffer)
    (let ((vc (ecard-create :fn "John Doe"
                            :adr '("" "" "456 Oak Ave" "Newtown" "CA" "90210" "USA"))))
      (ecard-sync-ecard-to-org vc)
      (should (equal (org-entry-get (point) "ADDRESS")
                     ";;456 Oak Ave;Newtown;CA;90210;USA")))
    (kill-buffer)))

(ert-deftest ecard-sync-test-ecard-to-org-org-as-list ()
  "Test ecard-to-org with ORG as list."
  (with-current-buffer (ecard-sync-test--create-org-buffer)
    (let ((vc (ecard-create :fn "John Doe"
                            :org '("ACME Corp"))))
      (ecard-sync-ecard-to-org vc)
      (should (equal (org-entry-get (point) "ORG") "ACME Corp")))
    (kill-buffer)))

(ert-deftest ecard-sync-test-ecard-to-org-org-as-string ()
  "Test ecard-to-org with ORG as string."
  (with-current-buffer (ecard-sync-test--create-org-buffer)
    (let ((vc (ecard-create :fn "John Doe")))
      ;; Set ORG with a string value directly
      (setf (ecard-org vc)
            (list (ecard-property :name "ORG" :value "String Corp")))
      (ecard-sync-ecard-to-org vc)
      (should (equal (org-entry-get (point) "ORG") "String Corp")))
    (kill-buffer)))

;;; Merge direction with equal timestamps

(ert-deftest ecard-sync-test-determine-prefer-local-equal-timestamps ()
  "Test merge direction when SYNCED and REV are equal.
Should prefer local (return t) since server unchanged."
  (let ((ecard-sync-prefer-local nil))
    ;; Same day, same time → server unchanged → prefer local
    (should (eq (ecard-sync--determine-prefer-local
                 "[2025-01-28 Tue 12:00]" "20250128T120000Z")
                t))))

;;; Merge with empty slots

(ert-deftest ecard-sync-test-merge-both-empty-slot ()
  "Test merging when both sides have empty slot."
  (let ((ecard-sync-prefer-local t)
        (local-vc (ecard-create :fn "Test"))
        (server-vc (ecard-create :fn "Test")))
    (let ((merged (ecard-sync--merge-ecards local-vc server-vc)))
      ;; Both have no email, merged should also have none
      (should (null (ecard-get-property-value merged 'email))))))

(ert-deftest ecard-sync-test-merge-extended-properties ()
  "Test that extended properties from server are preserved."
  (let ((ecard-sync-prefer-local t)
        (local-vc (ecard-create :fn "Test"))
        (server-vc (ecard-create :fn "Test")))
    ;; Add X-CUSTOM to server
    (setf (ecard-extended server-vc)
          '(("X-CUSTOM" . ((ecard-property :name "X-CUSTOM" :value "custom-val")))))
    (let ((merged (ecard-sync--merge-ecards local-vc server-vc)))
      ;; Server's extended properties should be used
      (should (ecard-extended merged)))))

;;; Name splitting edge cases

(ert-deftest ecard-sync-test-split-name-four-parts ()
  "Test splitting four-part names."
  (let ((result (ecard-sync--split-name "John Michael Scott Doe")))
    (should (equal (car result) "Doe"))
    (should (equal (cadr result) "John"))
    (should (string-match-p "Michael Scott" (nth 2 result)))))

(ert-deftest ecard-sync-test-split-name-with-whitespace ()
  "Test splitting names with extra whitespace."
  (let ((result (ecard-sync--split-name "  John   Doe  ")))
    (should (equal result '("Doe" "John" "" "" "")))))

;;; Sync-all-entries tests

(ert-deftest ecard-sync-test-all-entries-not-org-mode ()
  "Test sync-all-entries fails outside org-mode."
  (with-temp-buffer
    (should-error (ecard-sync-all-entries) :type 'user-error)))

;;; Get-addressbook tests

(ert-deftest ecard-sync-test-get-addressbook-not-configured ()
  "Test get-addressbook when server not configured."
  (let ((ecard-sync-server nil)
        (ecard-sync--addressbook-cache nil))
    (should-error (ecard-sync--get-addressbook) :type 'user-error)))

(ert-deftest ecard-sync-test-get-addressbook-uses-cache ()
  "Test get-addressbook returns cached value."
  (let ((ecard-sync--addressbook-cache 'cached-addressbook))
    (should (eq (ecard-sync--get-addressbook) 'cached-addressbook))))

;;; Sync entry tests

(ert-deftest ecard-sync-test-entry-not-org-mode ()
  "Test sync-entry fails outside org-mode."
  (with-temp-buffer
    (should-error (ecard-sync-entry) :type 'user-error)))

;;; Mock-based test utilities

(require 'ecard-carddav-mock)
(require 'ecard-carddav-sync)

(defvar ecard-sync-test--mock-server nil
  "Mock server for sync tests.")

(defun ecard-sync-test--setup-mock ()
  "Set up mock CardDAV server for sync tests.
Returns (server . addressbook) cons."
  ;; Create mock server
  (setq ecard-sync-test--mock-server
        (ecard-carddav-mock-server-create
         :base-url "https://test.example.com"))

  ;; Add address book
  (ecard-carddav-mock-add-addressbook
   ecard-sync-test--mock-server
   "/addressbooks/user/contacts/"
   "Test Contacts"
   "Test address book")

  ;; Install mock
  (ecard-carddav-mock-install ecard-sync-test--mock-server)

  ;; Create server and discover addressbooks
  (let* ((auth (ecard-carddav-auth-basic-create
                :username "user" :password "pass"))
         (server (ecard-carddav-server-create
                  :url "https://test.example.com"
                  :auth auth))
         (addressbooks (ecard-carddav-discover-addressbooks server))
         (ab (car addressbooks)))
    (cons server ab)))

(defun ecard-sync-test--teardown-mock ()
  "Tear down mock CardDAV server."
  (ecard-carddav-mock-uninstall)
  (setq ecard-sync-test--mock-server nil
        ecard-sync--addressbook-cache nil
        ecard-sync--resource-cache nil))

(defun ecard-sync-test--create-org-buffer-with-id (name id &optional props body)
  "Create org buffer with NAME as headline and ID.
PROPS is an alist of extra properties.  BODY is text after properties."
  (with-current-buffer (generate-new-buffer "*ecard-sync-test-mock*")
    (org-mode)
    (insert (format "* %s\n" name))
    (insert ":PROPERTIES:\n")
    (when id
      (insert (format ":ID:       %s\n" id)))
    (dolist (prop props)
      (insert (format ":%s:    %s\n" (car prop) (cdr prop))))
    (insert ":END:\n")
    (when body
      (insert body "\n"))
    (goto-char (point-min))
    (re-search-forward "^\\*")
    (beginning-of-line)
    (current-buffer)))

;;; Line 96 - multibyte body in fix-multibyte-request

(ert-deftest ecard-sync-test-fix-multibyte-request-multibyte-body ()
  "Test multibyte request fix when body is a multibyte string.
Covers line 96: (encode-coding-string saved-data \\='utf-8)."
  (require 'url-http)
  (let ((orig-data (and (boundp 'url-http-data) url-http-data)))
    (unwind-protect
        (progn
          ;; Set body to a multibyte string (not yet encoded to unibyte)
          (setq url-http-data (string-to-multibyte "FN:Bahá'í Contact\r\n"))
          (should (multibyte-string-p url-http-data))
          (let* ((orig-fn (lambda ()
                            "PUT /test HTTP/1.1\r\nHost: example.com\r\n\r\n"))
                 (result (ecard-sync--fix-multibyte-request orig-fn)))
            ;; Result must be unibyte
            (should (not (multibyte-string-p result)))
            (should (= (string-bytes result) (length result)))
            ;; Should contain both headers and body
            (let ((decoded (decode-coding-string result 'utf-8)))
              (should (string-match-p "Content-length:" decoded))
              (should (string-match-p "Bah" decoded)))))
      (setq url-http-data orig-data))))

;;; Lines 186-195 - ecard-sync--get-addressbook with server discovery

(ert-deftest ecard-sync-test-get-addressbook-discovers ()
  "Test get-addressbook discovers addressbooks from server.
Covers lines 186-195."
  (let ((pair (ecard-sync-test--setup-mock)))
    (unwind-protect
        (let* ((server (car pair))
               (ecard-sync-server server)
               (ecard-sync--addressbook-cache nil))
          (let ((ab (ecard-sync--get-addressbook)))
            (should ab)
            (should (ecard-carddav-addressbook-p ab))
            (should (string= (oref ab display-name) "Test Contacts"))))
      (ecard-sync-test--teardown-mock))))

(ert-deftest ecard-sync-test-get-addressbook-by-name ()
  "Test get-addressbook finds addressbook by name.
Covers lines 190-194 (the cl-find branch)."
  (let ((pair (ecard-sync-test--setup-mock)))
    (unwind-protect
        (let* ((server (car pair))
               (ecard-sync-server server)
               (ecard-sync--addressbook-cache nil)
               (ecard-sync-addressbook-name "Test Contacts"))
          (let ((ab (ecard-sync--get-addressbook)))
            (should ab)
            (should (string= (oref ab display-name) "Test Contacts"))))
      (ecard-sync-test--teardown-mock))))

(ert-deftest ecard-sync-test-get-addressbook-name-not-found ()
  "Test get-addressbook errors when named addressbook not found.
Covers line 194 (user-error branch)."
  (let ((pair (ecard-sync-test--setup-mock)))
    (unwind-protect
        (let* ((server (car pair))
               (ecard-sync-server server)
               (ecard-sync--addressbook-cache nil)
               (ecard-sync-addressbook-name "Nonexistent"))
          (should-error (ecard-sync--get-addressbook) :type 'user-error))
      (ecard-sync-test--teardown-mock))))

;;; Line 431 - ecard-to-org with address as string (not list)

(ert-deftest ecard-sync-test-ecard-to-org-address-string ()
  "Test ecard-to-org when address is a plain string, not a list.
Covers line 431 (the non-list adr branch)."
  (with-current-buffer (ecard-sync-test--create-org-buffer)
    (let ((vc (ecard-create :fn "John Doe")))
      ;; Set ADR with a plain string value directly
      (setf (ecard-adr vc)
            (list (ecard-property :name "ADR" :value "123 Main St, Springfield")))
      (ecard-sync-ecard-to-org vc)
      (should (equal (org-entry-get (point) "ADDRESS")
                     "123 Main St, Springfield")))
    (kill-buffer)))

;;; Line 448 - ecard-to-org deleting legacy NOTE property

(ert-deftest ecard-sync-test-ecard-to-org-removes-legacy-note ()
  "Test ecard-to-org removes legacy :NOTE: property when note is body text.
Covers line 448 (org-delete-property \"NOTE\")."
  (with-current-buffer (generate-new-buffer "*ecard-sync-test-note*")
    (org-mode)
    (insert "* Test Person\n")
    (insert ":PROPERTIES:\n")
    (insert ":NOTE:     Legacy note value\n")
    (insert ":END:\n")
    (insert "Old body\n")
    (goto-char (point-min))
    (re-search-forward "^\\*")
    (beginning-of-line)
    (let ((vc (ecard-create :fn "Test Person"
                            :note "New note from server")))
      (ecard-sync-ecard-to-org vc)
      ;; Legacy NOTE property should be deleted
      (should (null (org-entry-get (point) "NOTE")))
      ;; Body text should be the new note
      (let ((vc2 (ecard-sync-org-to-ecard)))
        (should (equal (ecard-get-property-value vc2 'note)
                       "New note from server"))))
    (kill-buffer)))

;;; Lines 475-496 - ecard-sync--ensure-resources and find-resource-by-uid

(ert-deftest ecard-sync-test-ensure-resources ()
  "Test ensure-resources fetches and caches all vCards.
Covers lines 475-486."
  (let ((pair (ecard-sync-test--setup-mock)))
    (unwind-protect
        (let* ((server (car pair))
               (ab (cdr pair))
               (ecard-sync--resource-cache nil))
          ;; Add a vCard to the mock server
          (let ((vc (ecard-create :fn "Alice Test"
                                  :uid "alice-uid-123")))
            (ecard-carddav-put-ecard
             ab "/addressbooks/user/contacts/alice-uid-123.vcf" vc))

          ;; Ensure resources fetches them
          (let ((entries (ecard-sync--ensure-resources ab)))
            (should entries)
            (should (listp entries))
            (should (>= (length entries) 1))
            ;; Each entry is (resource . ecard)
            (let ((entry (car entries)))
              (should (ecard-carddav-resource-p (car entry)))
              (should (ecard-p (cdr entry))))))
      (ecard-sync-test--teardown-mock))))

(ert-deftest ecard-sync-test-ensure-resources-empty ()
  "Test ensure-resources with empty addressbook.
Covers lines 478-479 (empty branch)."
  (let ((pair (ecard-sync-test--setup-mock)))
    (unwind-protect
        (let* ((ab (cdr pair))
               (ecard-sync--resource-cache nil))
          ;; No vCards added - should return nil
          (let ((entries (ecard-sync--ensure-resources ab)))
            (should (null entries))
            ;; Cache should be 'empty
            (should (eq ecard-sync--resource-cache 'empty))))
      (ecard-sync-test--teardown-mock))))

(ert-deftest ecard-sync-test-ensure-resources-cached ()
  "Test ensure-resources returns cached results.
Covers lines 484-486 (cached path)."
  (let ((pair (ecard-sync-test--setup-mock)))
    (unwind-protect
        (let* ((ab (cdr pair))
               (ecard-sync--resource-cache '((fake-resource . fake-ecard))))
          ;; Should return cached value without network call
          (let ((entries (ecard-sync--ensure-resources ab)))
            (should (equal entries '((fake-resource . fake-ecard))))))
      (ecard-sync-test--teardown-mock))))

(ert-deftest ecard-sync-test-find-resource-by-uid ()
  "Test finding resource by UID suffix matching.
Covers lines 488-496."
  (let ((pair (ecard-sync-test--setup-mock)))
    (unwind-protect
        (let* ((ab (cdr pair))
               (ecard-sync--resource-cache nil))
          ;; Add a vCard
          (let ((vc (ecard-create :fn "Bob Test"
                                  :uid "bob-uid-456")))
            (ecard-carddav-put-ecard
             ab "/addressbooks/user/contacts/bob-uid-456.vcf" vc))

          ;; Find by UID
          (let ((match (ecard-sync--find-resource-by-uid ab "bob-uid-456")))
            (should match)
            (should (ecard-carddav-resource-p (car match)))
            (should (ecard-p (cdr match)))))
      (ecard-sync-test--teardown-mock))))

(ert-deftest ecard-sync-test-find-resource-by-uid-not-found ()
  "Test find-resource-by-uid returns nil for unknown UID."
  (let ((pair (ecard-sync-test--setup-mock)))
    (unwind-protect
        (let* ((ab (cdr pair))
               (ecard-sync--resource-cache nil))
          ;; Add a vCard
          (let ((vc (ecard-create :fn "Bob Test"
                                  :uid "bob-uid-456")))
            (ecard-carddav-put-ecard
             ab "/addressbooks/user/contacts/bob-uid-456.vcf" vc))

          ;; Search for non-existent UID
          (let ((match (ecard-sync--find-resource-by-uid ab "nonexistent-uid")))
            (should (null match))))
      (ecard-sync-test--teardown-mock))))

;;; Lines 498-529 - ecard-sync--find-resource-by-fn

(ert-deftest ecard-sync-test-find-resource-by-fn ()
  "Test finding resource by formatted name.
Covers lines 498-513."
  (let ((pair (ecard-sync-test--setup-mock)))
    (unwind-protect
        (let* ((ab (cdr pair))
               (ecard-sync--resource-cache nil))
          ;; Add vCards
          (let ((vc1 (ecard-create :fn "Charlie Test"
                                   :uid "charlie-uid")))
            (ecard-carddav-put-ecard
             ab "/addressbooks/user/contacts/charlie-uid.vcf" vc1))

          ;; Find by FN (case-insensitive)
          (let ((match (ecard-sync--find-resource-by-fn ab "charlie test")))
            (should match)
            (should (ecard-carddav-resource-p (car match)))
            (should (equal (ecard-get-property-value (cdr match) 'fn)
                           "Charlie Test"))))
      (ecard-sync-test--teardown-mock))))

(ert-deftest ecard-sync-test-find-resource-by-fn-not-found ()
  "Test find-resource-by-fn returns nil for unknown name.
Covers line 512."
  (let ((pair (ecard-sync-test--setup-mock)))
    (unwind-protect
        (let* ((ab (cdr pair))
               (ecard-sync--resource-cache nil))
          ;; Add a vCard
          (let ((vc (ecard-create :fn "Charlie Test"
                                  :uid "charlie-uid")))
            (ecard-carddav-put-ecard
             ab "/addressbooks/user/contacts/charlie-uid.vcf" vc))

          ;; Search for non-existent FN
          (let ((match (ecard-sync--find-resource-by-fn ab "Unknown Person")))
            (should (null match))))
      (ecard-sync-test--teardown-mock))))

(ert-deftest ecard-sync-test-find-resource-by-fn-multiple-matches ()
  "Test find-resource-by-fn with multiple matches prompts user.
Covers lines 514-529."
  (let ((pair (ecard-sync-test--setup-mock)))
    (unwind-protect
        (let* ((ab (cdr pair))
               (ecard-sync--resource-cache nil))
          ;; Add two vCards with the same FN
          (let ((vc1 (ecard-create :fn "Duplicate Name"
                                   :uid "dup-uid-1"
                                   :email "dup1@example.com"))
                (vc2 (ecard-create :fn "Duplicate Name"
                                   :uid "dup-uid-2"
                                   :email "dup2@example.com")))
            (ecard-carddav-put-ecard
             ab "/addressbooks/user/contacts/dup-uid-1.vcf" vc1)
            (ecard-carddav-put-ecard
             ab "/addressbooks/user/contacts/dup-uid-2.vcf" vc2))

          ;; Mock completing-read to pick first match
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (_prompt choices &rest _)
                       (car choices))))
            (let ((match (ecard-sync--find-resource-by-fn ab "Duplicate Name")))
              (should match)
              (should (ecard-carddav-resource-p (car match)))
              (should (equal (ecard-get-property-value (cdr match) 'fn)
                             "Duplicate Name")))))
      (ecard-sync-test--teardown-mock))))

;;; Lines 612-694 - ecard-sync-entry (full sync command)

(ert-deftest ecard-sync-test-entry-new-contact ()
  "Test sync-entry creates new vCard on server when contact not found.
Covers lines 612-694 (case 1: has org-id, not on server, not found by FN)."
  (let ((pair (ecard-sync-test--setup-mock)))
    (unwind-protect
        (let* ((server (car pair))
               (ab (cdr pair))
               (ecard-sync-server server)
               (ecard-sync--addressbook-cache ab)
               (ecard-sync--resource-cache nil)
               (ecard-sync-prefer-local t))
          (with-current-buffer
              (ecard-sync-test--create-org-buffer-with-id
               "New Contact" "new-contact-uid-123"
               '(("EMAIL" . "new@example.com")
                 ("PHONE" . "+1-555-0000")))
            (unwind-protect
                (progn
                  (ecard-sync-entry)
                  ;; Should have set SYNCED property
                  (should (org-entry-get (point) "SYNCED"))
                  ;; ID should still be the same
                  (should (equal (org-entry-get (point) "ID")
                                 "new-contact-uid-123")))
              (kill-buffer))))
      (ecard-sync-test--teardown-mock))))

(ert-deftest ecard-sync-test-entry-existing-by-uid ()
  "Test sync-entry merges when contact found on server by UID.
Covers the merge branch of lines 629-634."
  (let ((pair (ecard-sync-test--setup-mock)))
    (unwind-protect
        (let* ((server (car pair))
               (ab (cdr pair))
               (ecard-sync-server server)
               (ecard-sync--addressbook-cache ab)
               (ecard-sync--resource-cache nil)
               (ecard-sync-prefer-local t))
          ;; First, put a vCard on the server
          (let ((server-vc (ecard-create
                            :fn "Existing Contact"
                            :uid "existing-uid-789"
                            :email "server@example.com"
                            :note "Server note")))
            (ecard-carddav-put-ecard
             ab "/addressbooks/user/contacts/existing-uid-789.vcf"
             server-vc))

          ;; Clear resource cache so ensure-resources fetches fresh
          (setq ecard-sync--resource-cache nil)

          (with-current-buffer
              (ecard-sync-test--create-org-buffer-with-id
               "Existing Contact" "existing-uid-789"
               '(("EMAIL" . "local@example.com")
                 ("PHONE" . "+1-555-1234")))
            (unwind-protect
                (progn
                  (ecard-sync-entry)
                  ;; Should have SYNCED property
                  (should (org-entry-get (point) "SYNCED"))
                  ;; ID should remain the same
                  (should (equal (org-entry-get (point) "ID")
                                 "existing-uid-789")))
              (kill-buffer))))
      (ecard-sync-test--teardown-mock))))

(ert-deftest ecard-sync-test-entry-found-by-fn ()
  "Test sync-entry adopts server UID when contact found by FN.
Covers lines 636-643 (found by FN, adopt server UID)."
  (let ((pair (ecard-sync-test--setup-mock)))
    (unwind-protect
        (let* ((server (car pair))
               (ab (cdr pair))
               (ecard-sync-server server)
               (ecard-sync--addressbook-cache ab)
               (ecard-sync--resource-cache nil)
               (ecard-sync-prefer-local t))
          ;; Put a vCard on server with different UID but same name
          (let ((server-vc (ecard-create
                            :fn "Found By Name"
                            :uid "server-uid-abc"
                            :email "server@example.com")))
            (ecard-carddav-put-ecard
             ab "/addressbooks/user/contacts/server-uid-abc.vcf"
             server-vc))

          (setq ecard-sync--resource-cache nil)

          ;; Create org entry with different ID but same name
          (with-current-buffer
              (ecard-sync-test--create-org-buffer-with-id
               "Found By Name" "local-uid-xyz"
               '(("EMAIL" . "local@example.com")))
            (unwind-protect
                (progn
                  (ecard-sync-entry)
                  ;; ID should be changed to server's UID
                  (should (equal (org-entry-get (point) "ID")
                                 "server-uid-abc"))
                  ;; Should have SYNCED property
                  (should (org-entry-get (point) "SYNCED")))
              (kill-buffer))))
      (ecard-sync-test--teardown-mock))))

(ert-deftest ecard-sync-test-entry-no-id-found-by-fn ()
  "Test sync-entry with no org ID, found on server by FN.
Covers lines 647-655 (case 2: no org-id, found by FN)."
  (let ((pair (ecard-sync-test--setup-mock)))
    (unwind-protect
        (let* ((server (car pair))
               (ab (cdr pair))
               (ecard-sync-server server)
               (ecard-sync--addressbook-cache ab)
               (ecard-sync--resource-cache nil)
               (ecard-sync-prefer-local t))
          ;; Put a vCard on server
          (let ((server-vc (ecard-create
                            :fn "No ID Person"
                            :uid "server-no-id-uid"
                            :email "server@example.com")))
            (ecard-carddav-put-ecard
             ab "/addressbooks/user/contacts/server-no-id-uid.vcf"
             server-vc))

          (setq ecard-sync--resource-cache nil)

          ;; Create org entry WITHOUT an ID
          (with-current-buffer
              (ecard-sync-test--create-org-buffer-with-id
               "No ID Person" nil
               '(("EMAIL" . "local@example.com")))
            (unwind-protect
                (progn
                  (ecard-sync-entry)
                  ;; Should adopt server's UID
                  (should (equal (org-entry-get (point) "ID")
                                 "server-no-id-uid"))
                  (should (org-entry-get (point) "SYNCED")))
              (kill-buffer))))
      (ecard-sync-test--teardown-mock))))

(ert-deftest ecard-sync-test-entry-no-id-new-contact ()
  "Test sync-entry with no org ID and not found on server.
Covers lines 656-659 (case 2: no org-id, not found, generate new UID)."
  (let ((pair (ecard-sync-test--setup-mock)))
    (unwind-protect
        (let* ((server (car pair))
               (ab (cdr pair))
               (ecard-sync-server server)
               (ecard-sync--addressbook-cache ab)
               (ecard-sync--resource-cache nil)
               (ecard-sync-prefer-local t))
          ;; Create org entry without an ID, no matching vCard on server
          (with-current-buffer
              (ecard-sync-test--create-org-buffer-with-id
               "Brand New Person" nil
               '(("EMAIL" . "brandnew@example.com")))
            (unwind-protect
                (progn
                  (ecard-sync-entry)
                  ;; Should have generated a new UID
                  (let ((new-id (org-entry-get (point) "ID")))
                    (should new-id)
                    (should (not (string-empty-p new-id))))
                  (should (org-entry-get (point) "SYNCED")))
              (kill-buffer))))
      (ecard-sync-test--teardown-mock))))

;;; Lines 697-715 - ecard-sync-all-entries

(ert-deftest ecard-sync-test-all-entries-syncs-multiple ()
  "Test sync-all-entries processes multiple org headings.
Covers lines 697-715."
  (let ((pair (ecard-sync-test--setup-mock)))
    (unwind-protect
        (let* ((server (car pair))
               (ab (cdr pair))
               (ecard-sync-server server)
               (ecard-sync--addressbook-cache ab)
               (ecard-sync--resource-cache nil)
               (ecard-sync-prefer-local t)
               (messages nil))
          (with-current-buffer (generate-new-buffer "*ecard-sync-test-all*")
            (org-mode)
            (insert "* Person One\n")
            (insert ":PROPERTIES:\n")
            (insert ":ID:       all-uid-001\n")
            (insert ":EMAIL:    one@example.com\n")
            (insert ":END:\n")
            (insert "* Person Two\n")
            (insert ":PROPERTIES:\n")
            (insert ":ID:       all-uid-002\n")
            (insert ":EMAIL:    two@example.com\n")
            (insert ":END:\n")
            (unwind-protect
                (progn
                  ;; Capture final message
                  (cl-letf (((symbol-function 'message)
                             (lambda (fmt &rest args)
                               (push (apply #'format fmt args) messages))))
                    (ecard-sync-all-entries))
                  ;; Should have synced 2 entries
                  (should (cl-some (lambda (m) (string-match-p "Synced 2 entries" m))
                                   messages)))
              (kill-buffer))))
      (ecard-sync-test--teardown-mock))))

(ert-deftest ecard-sync-test-all-entries-handles-errors ()
  "Test sync-all-entries counts errors gracefully.
Covers lines 708-714 (error handling branch)."
  (let ((pair (ecard-sync-test--setup-mock)))
    (unwind-protect
        (let* ((server (car pair))
               (ab (cdr pair))
               (ecard-sync-server server)
               (ecard-sync--addressbook-cache ab)
               (ecard-sync--resource-cache nil)
               (ecard-sync-prefer-local t)
               (messages nil)
               (call-count 0))
          (with-current-buffer (generate-new-buffer "*ecard-sync-test-err*")
            (org-mode)
            (insert "* Good Entry\n")
            (insert ":PROPERTIES:\n")
            (insert ":ID:       err-uid-001\n")
            (insert ":END:\n")
            (insert "* Bad Entry\n")
            (insert ":PROPERTIES:\n")
            (insert ":ID:       err-uid-002\n")
            (insert ":END:\n")
            (unwind-protect
                (progn
                  ;; Make the second call to ecard-sync-entry fail
                  (cl-letf (((symbol-function 'ecard-sync-entry)
                             (lambda ()
                               (setq call-count (1+ call-count))
                               (when (= call-count 2)
                                 (error "Simulated sync failure"))))
                            ((symbol-function 'message)
                             (lambda (fmt &rest args)
                               (push (apply #'format fmt args) messages))))
                    (ecard-sync-all-entries))
                  ;; Should report 1 success and 1 error
                  (should (cl-some (lambda (m) (string-match-p "1 entries.*1 errors" m))
                                   messages)))
              (kill-buffer))))
      (ecard-sync-test--teardown-mock))))

;;; Timestamp-based merge direction integration

(ert-deftest ecard-sync-test-entry-timestamp-merge-direction ()
  "Test that sync-entry uses SYNCED/REV timestamps for merge direction.
When server REV is newer than SYNCED, server values should win."
  (let ((pair (ecard-sync-test--setup-mock)))
    (unwind-protect
        (let* ((server (car pair))
               (ab (cdr pair))
               (ecard-sync-server server)
               (ecard-sync--addressbook-cache ab)
               (ecard-sync--resource-cache nil)
               (ecard-sync-prefer-local t))
          ;; Put a vCard on the server with a recent REV
          (let ((server-vc (ecard-create
                            :fn "Timestamp Test"
                            :uid "ts-uid-001"
                            :email "server-new@example.com")))
            (ecard-set-property server-vc 'rev "20260215T120000Z")
            (ecard-carddav-put-ecard
             ab "/addressbooks/user/contacts/ts-uid-001.vcf"
             server-vc))

          (setq ecard-sync--resource-cache nil)

          ;; Create org entry with old SYNCED timestamp
          (with-current-buffer
              (ecard-sync-test--create-org-buffer-with-id
               "Timestamp Test" "ts-uid-001"
               '(("EMAIL" . "local-old@example.com")
                 ("SYNCED" . "[2026-01-01 Thu 12:00]")))
            (unwind-protect
                (progn
                  (ecard-sync-entry)
                  ;; Server was newer, so server email should win
                  (should (equal (org-entry-get (point) "EMAIL")
                                 "server-new@example.com")))
              (kill-buffer))))
      (ecard-sync-test--teardown-mock))))

(provide 'ecard-sync-test)
;;; ecard-sync-test.el ends here
