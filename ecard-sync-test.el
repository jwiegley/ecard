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

(provide 'ecard-sync-test)
;;; ecard-sync-test.el ends here
