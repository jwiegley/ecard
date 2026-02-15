;;; ecard-compat-test.el --- Tests for ecard-compat -*- lexical-binding: t; -*-

;; Copyright (C) 2025 John Wiegley

;; Author: John Wiegley <johnw@gnu.org>

;;; Commentary:

;; Tests for ecard-compat.el - vCard 2.1/3.0 compatibility layer

;;; Code:

(require 'ert)
(require 'ecard)
(require 'ecard-compat)

;;; Version detection tests

(ert-deftest ecard-compat-detect-version-21 ()
  "Test detection of vCard 2.1."
  (should (eq (ecard-compat--detect-version "VERSION:2.1") 'v21)))

(ert-deftest ecard-compat-detect-version-30 ()
  "Test detection of vCard 3.0."
  (should (eq (ecard-compat--detect-version "VERSION:3.0") 'v30)))

(ert-deftest ecard-compat-detect-version-40 ()
  "Test detection of vCard 4.0."
  (should (eq (ecard-compat--detect-version "VERSION:4.0") 'v40)))

(ert-deftest ecard-compat-detect-version-unknown ()
  "Test detection of unknown version."
  (should (null (ecard-compat--detect-version "VERSION:5.0"))))

;;; Encoding tests

(ert-deftest ecard-compat-decode-base64 ()
  "Test BASE64 decoding."
  (should (string= (ecard-compat--decode-base64 "SGVsbG8gV29ybGQ=")
                   "Hello World")))

(ert-deftest ecard-compat-decode-quoted-printable ()
  "Test QUOTED-PRINTABLE decoding."
  (should (string= (ecard-compat--decode-quoted-printable "Hello=20World")
                   "Hello World"))
  (should (string= (ecard-compat--decode-quoted-printable "Line=0ABreak")
                   "Line\nBreak")))

(ert-deftest ecard-compat-decode-quoted-printable-soft-break ()
  "Test QUOTED-PRINTABLE soft line break handling."
  (should (string= (ecard-compat--decode-quoted-printable "Long=\r\nLine")
                   "LongLine")))

;;; Parameter conversion tests

(ert-deftest ecard-compat-convert-params-21-types ()
  "Test vCard 2.1 type parameter conversion."
  (let* ((params '(("HOME" . t) ("VOICE" . t)))
         (result (ecard-compat--convert-params-21 params "TEL"))
         (converted-params (plist-get result :params)))
    (should (member '("TYPE" . "home,voice") converted-params))))

(ert-deftest ecard-compat-convert-params-21-encoding ()
  "Test vCard 2.1 encoding parameter extraction."
  (let* ((params '(("ENCODING" . "BASE64") ("TYPE" . "JPEG")))
         (result (ecard-compat--convert-params-21 params "PHOTO")))
    (should (string= (plist-get result :encoding) "BASE64"))
    (should (member '("TYPE" . "JPEG") (plist-get result :params)))))

(ert-deftest ecard-compat-convert-params-30-type ()
  "Test vCard 3.0 type parameter conversion."
  (let* ((params '(("TYPE" . "HOME,WORK")))
         (result (ecard-compat--convert-params-30 params "TEL"))
         (converted-params (plist-get result :params)))
    (should (member '("TYPE" . "home,work") converted-params))))

;;; Property filtering tests

(ert-deftest ecard-compat-should-include-property ()
  "Test property filtering."
  (should (ecard-compat--should-include-property-p "TEL"))
  (should (ecard-compat--should-include-property-p "EMAIL"))
  (should-not (ecard-compat--should-include-property-p "LABEL"))
  (should-not (ecard-compat--should-include-property-p "MAILER"))
  (should-not (ecard-compat--should-include-property-p "CLASS")))

;;; Media type detection tests

(ert-deftest ecard-compat-detect-media-type-photo ()
  "Test media type detection for PHOTO."
  (should (string= (ecard-compat--detect-media-type "PHOTO" nil)
                   "image/jpeg")))

(ert-deftest ecard-compat-detect-media-type-from-param ()
  "Test media type detection from TYPE parameter."
  (should (string= (ecard-compat--detect-media-type
                   "PHOTO" '(("TYPE" . "image/png")))
                   "image/png")))

;;; vCard 2.1 parsing tests

(ert-deftest ecard-compat-parse-21-simple ()
  "Test simple vCard 2.1 parsing."
  (let* ((ecard-21 "BEGIN:VCARD
VERSION:2.1
FN:John Doe
TEL;HOME;VOICE:555-1234
EMAIL;INTERNET:john@example.com
END:VCARD")
         (vc (ecard-compat-parse-21 ecard-21)))

    (should (ecard-p vc))
    (should (string= (ecard-get-property-value vc 'fn) "John Doe"))

    ;; Check telephone
    (let ((tel-props (slot-value vc 'tel)))
      (should (= (length tel-props) 1))
      (should (string= (oref (car tel-props) value) "555-1234"))
      (let ((params (oref (car tel-props) parameters)))
        (should (assoc "TYPE" params))
        (should (string-match-p "home" (cdr (assoc "TYPE" params))))
        (should (string-match-p "voice" (cdr (assoc "TYPE" params))))))

    ;; Check email
    (let ((email-props (slot-value vc 'email)))
      (should (= (length email-props) 1))
      (should (string= (oref (car email-props) value) "john@example.com")))))

(ert-deftest ecard-compat-parse-21-base64-photo ()
  "Test vCard 2.1 with BASE64 encoded photo."
  (let* ((ecard-21 "BEGIN:VCARD
VERSION:2.1
FN:John Doe
PHOTO;ENCODING=BASE64;TYPE=JPEG:SGVsbG8gV29ybGQ=
END:VCARD")
         (vc (ecard-compat-parse-21 ecard-21)))

    (should (ecard-p vc))
    (let ((photo-props (slot-value vc 'photo)))
      (should (= (length photo-props) 1))
      (let ((photo-value (oref (car photo-props) value)))
        ;; Should be converted to data URI
        (should (string-prefix-p "data:image/jpeg;base64," photo-value))))))

(ert-deftest ecard-compat-parse-21-quoted-printable ()
  "Test vCard 2.1 with QUOTED-PRINTABLE encoding."
  (let* ((ecard-21 "BEGIN:VCARD
VERSION:2.1
FN:John Doe
NOTE;ENCODING=QUOTED-PRINTABLE:Hello=20World
END:VCARD")
         (vc (ecard-compat-parse-21 ecard-21)))

    (should (ecard-p vc))
    (should (string= (ecard-get-property-value vc 'note) "Hello World"))))

(ert-deftest ecard-compat-parse-21-charset ()
  "Test vCard 2.1 with character set conversion."
  (let* ((ecard-21 "BEGIN:VCARD
VERSION:2.1
FN:John Doe
NOTE;CHARSET=UTF-8:Test Note
END:VCARD")
         (vc (ecard-compat-parse-21 ecard-21)))

    (should (ecard-p vc))
    (should (string= (ecard-get-property-value vc 'note) "Test Note"))))

(ert-deftest ecard-compat-parse-21-structured-name ()
  "Test vCard 2.1 with structured name."
  (let* ((ecard-21 "BEGIN:VCARD
VERSION:2.1
FN:John Doe
N:Doe;John;Q;Mr;Jr
END:VCARD")
         (vc (ecard-compat-parse-21 ecard-21)))

    (should (ecard-p vc))
    (let ((n-value (ecard-get-property-value vc 'n)))
      (should (listp n-value))
      (should (equal n-value '("Doe" "John" "Q" "Mr" "Jr"))))))

(ert-deftest ecard-compat-parse-21-address ()
  "Test vCard 2.1 with structured address."
  (let* ((ecard-21 "BEGIN:VCARD
VERSION:2.1
FN:John Doe
ADR;HOME:;;123 Main St;City;State;12345;Country
END:VCARD")
         (vc (ecard-compat-parse-21 ecard-21)))

    (should (ecard-p vc))
    (let ((adr-props (slot-value vc 'adr)))
      (should (= (length adr-props) 1))
      (let ((adr-value (oref (car adr-props) value)))
        (should (listp adr-value))
        (should (equal (nth 2 adr-value) "123 Main St"))
        (should (equal (nth 3 adr-value) "City"))))))

(ert-deftest ecard-compat-parse-21-drops-label ()
  "Test that LABEL property is dropped."
  (let* ((ecard-21 "BEGIN:VCARD
VERSION:2.1
FN:John Doe
LABEL;HOME:123 Main St\\nCity, State 12345
END:VCARD")
         (vc (ecard-compat-parse-21 ecard-21)))

    (should (ecard-p vc))
    ;; LABEL should not exist in vCard 4.0
    ;; Since ecard class doesn't have a label slot, this is automatically dropped
    (should (string= (ecard-get-property-value vc 'fn) "John Doe"))))

;;; vCard 3.0 parsing tests

(ert-deftest ecard-compat-parse-30-simple ()
  "Test simple vCard 3.0 parsing."
  (let* ((ecard-30 "BEGIN:VCARD
VERSION:3.0
FN:Jane Smith
TEL;TYPE=HOME,VOICE:555-5678
EMAIL;TYPE=INTERNET:jane@example.com
END:VCARD")
         (vc (ecard-compat-parse-30 ecard-30)))

    (should (ecard-p vc))
    (should (string= (ecard-get-property-value vc 'fn) "Jane Smith"))

    ;; Check telephone
    (let ((tel-props (slot-value vc 'tel)))
      (should (= (length tel-props) 1))
      (should (string= (oref (car tel-props) value) "555-5678"))
      (let ((params (oref (car tel-props) parameters)))
        (should (assoc "TYPE" params))
        (should (string-match-p "home" (cdr (assoc "TYPE" params))))
        (should (string-match-p "voice" (cdr (assoc "TYPE" params))))))))

(ert-deftest ecard-compat-parse-30-org ()
  "Test vCard 3.0 with organization."
  (let* ((ecard-30 "BEGIN:VCARD
VERSION:3.0
FN:Jane Smith
ORG:Example Corp;Engineering
END:VCARD")
         (vc (ecard-compat-parse-30 ecard-30)))

    (should (ecard-p vc))
    (let ((org-value (ecard-get-property-value vc 'org)))
      (should (listp org-value))
      (should (equal org-value '("Example Corp" "Engineering"))))))

(ert-deftest ecard-compat-parse-30-categories ()
  "Test vCard 3.0 with categories (text-list)."
  (let* ((ecard-30 "BEGIN:VCARD
VERSION:3.0
FN:Jane Smith
CATEGORIES:Work,Friend,VIP
END:VCARD")
         (vc (ecard-compat-parse-30 ecard-30)))

    (should (ecard-p vc))
    (let ((cat-value (ecard-get-property-value vc 'categories)))
      (should (listp cat-value))
      (should (equal cat-value '("Work" "Friend" "VIP"))))))

;;; Auto-detection tests

(ert-deftest ecard-compat-parse-auto-21 ()
  "Test auto-detection of vCard 2.1."
  (let* ((ecard-21 "BEGIN:VCARD
VERSION:2.1
FN:Auto Test
END:VCARD")
         (vc (ecard-compat-parse ecard-21)))

    (should (ecard-p vc))
    (should (string= (ecard-get-property-value vc 'fn) "Auto Test"))))

(ert-deftest ecard-compat-parse-auto-30 ()
  "Test auto-detection of vCard 3.0."
  (let* ((ecard-30 "BEGIN:VCARD
VERSION:3.0
FN:Auto Test
END:VCARD")
         (vc (ecard-compat-parse ecard-30)))

    (should (ecard-p vc))
    (should (string= (ecard-get-property-value vc 'fn) "Auto Test"))))

(ert-deftest ecard-compat-parse-auto-40 ()
  "Test auto-detection of vCard 4.0."
  (let* ((ecard-40 "BEGIN:VCARD
VERSION:4.0
FN:Auto Test
END:VCARD")
         (vc (ecard-compat-parse ecard-40)))

    (should (ecard-p vc))
    (should (string= (ecard-get-property-value vc 'fn) "Auto Test"))))

(ert-deftest ecard-compat-parse-auto-unknown ()
  "Test error on unknown version."
  (should-error
   (ecard-compat-parse "BEGIN:VCARD\nVERSION:5.0\nFN:Test\nEND:VCARD")
   :type 'ecard-compat-version-error))

;;; Multiple vCard parsing tests

(ert-deftest ecard-compat-parse-multiple-mixed ()
  "Test parsing multiple vCards with different versions."
  (let* ((vcards-text "BEGIN:VCARD
VERSION:2.1
FN:John Doe
END:VCARD
BEGIN:VCARD
VERSION:3.0
FN:Jane Smith
END:VCARD
BEGIN:VCARD
VERSION:4.0
FN:Bob Johnson
END:VCARD")
         (vcards (ecard-compat-parse-multiple vcards-text)))

    (should (= (length vcards) 3))
    (should (string= (ecard-get-property-value (nth 0 vcards) 'fn) "John Doe"))
    (should (string= (ecard-get-property-value (nth 1 vcards) 'fn) "Jane Smith"))
    (should (string= (ecard-get-property-value (nth 2 vcards) 'fn) "Bob Johnson"))))

;;; Data URI creation tests

(ert-deftest ecard-compat-create-data-uri ()
  "Test data URI creation."
  (let ((uri (ecard-compat--create-data-uri "SGVsbG8=" "image/jpeg")))
    (should (string= uri "data:image/jpeg;base64,SGVsbG8="))))

;;; Character set conversion tests

(ert-deftest ecard-compat-convert-charset-utf8 ()
  "Test charset conversion for UTF-8 (should be no-op)."
  (should (string= (ecard-compat--convert-charset "Test" "UTF-8") "Test"))
  (should (string= (ecard-compat--convert-charset "Test" "utf-8") "Test")))

(ert-deftest ecard-compat-convert-charset-nil ()
  "Test charset conversion for nil charset (should be no-op)."
  (should (string= (ecard-compat--convert-charset "Test" nil) "Test")))

;;; Type value mapping tests

(ert-deftest ecard-compat-map-type-value-known ()
  "Test mapping of known type values."
  (should (string= (ecard-compat--map-type-value "HOME") "home"))
  (should (string= (ecard-compat--map-type-value "WORK") "work"))
  (should (string= (ecard-compat--map-type-value "CELL") "cell")))

(ert-deftest ecard-compat-map-type-value-dropped ()
  "Test mapping of dropped type values."
  (should (null (ecard-compat--map-type-value "INTERNET")))
  (should (null (ecard-compat--map-type-value "DOM"))))

(ert-deftest ecard-compat-map-type-value-unknown ()
  "Test mapping of unknown type values (pass through as lowercase)."
  (should (string= (ecard-compat--map-type-value "CUSTOM") "custom")))

;;; Integration tests

(ert-deftest ecard-compat-parse-21-full-example ()
  "Test full vCard 2.1 example."
  (let* ((ecard-21 "BEGIN:VCARD
VERSION:2.1
FN:John Q. Public
N:Public;John;Quinlan;Mr.;Esq.
TEL;HOME;VOICE:555-1234
TEL;WORK;FAX:555-5678
EMAIL;INTERNET:john@example.com
ADR;WORK:;;100 Main St;Suite 200;Springfield;IL;62701;USA
ORG:Example Corp;Engineering
TITLE:Software Engineer
BDAY:1980-01-15
NOTE:This is a note\\nwith a line break
END:VCARD")
         (vc (ecard-compat-parse-21 ecard-21)))

    (should (ecard-p vc))
    (should (string= (ecard-get-property-value vc 'fn) "John Q. Public"))

    ;; Check structured name
    (let ((n-value (ecard-get-property-value vc 'n)))
      (should (equal n-value '("Public" "John" "Quinlan" "Mr." "Esq."))))

    ;; Check multiple phones
    (let ((tel-props (slot-value vc 'tel)))
      (should (= (length tel-props) 2)))

    ;; Check email
    (should (string= (ecard-get-property-value vc 'email) "john@example.com"))

    ;; Check organization
    (let ((org-value (ecard-get-property-value vc 'org)))
      (should (equal org-value '("Example Corp" "Engineering"))))

    ;; Check title
    (should (string= (ecard-get-property-value vc 'title) "Software Engineer"))

    ;; Check birthday
    (should (string= (ecard-get-property-value vc 'bday) "1980-01-15"))

    ;; Check note with escaped newline
    (should (string-match-p "line break" (ecard-get-property-value vc 'note)))))

(ert-deftest ecard-compat-roundtrip-21-to-40 ()
  "Test roundtrip from vCard 2.1 to 4.0 and back."
  (let* ((ecard-21 "BEGIN:VCARD
VERSION:2.1
FN:Test User
N:User;Test;;;
TEL;HOME:555-1234
EMAIL:test@example.com
END:VCARD")
         (vc (ecard-compat-parse-21 ecard-21))
         (serialized (ecard-serialize vc)))

    (should (ecard-p vc))
    (should (string-match-p "VERSION:4.0" serialized))
    (should (string-match-p "FN:Test User" serialized))

    ;; Parse the serialized 4.0 version
    (let ((vc2 (ecard-parse serialized)))
      (should (ecard-p vc2))
      (should (string= (ecard-get-property-value vc2 'fn) "Test User")))))

;;; ecard-compat-parse-buffer tests

(ert-deftest ecard-compat-parse-buffer-single ()
  "Test parsing single vCard from buffer returns single object."
  (with-temp-buffer
    (insert "BEGIN:VCARD
VERSION:3.0
FN:Single Test
EMAIL:single@example.com
END:VCARD")
    (let ((result (ecard-compat-parse-buffer)))
      (should (ecard-p result))
      (should-not (listp result))
      (should (string= "Single Test" (ecard-get-property-value result 'fn)))
      (should (string= "single@example.com" (ecard-get-property-value result 'email))))))

(ert-deftest ecard-compat-parse-buffer-multiple ()
  "Test parsing multiple vCards from buffer returns list."
  (with-temp-buffer
    (insert "BEGIN:VCARD
VERSION:2.1
FN:First Contact
EMAIL:first@example.com
END:VCARD
BEGIN:VCARD
VERSION:3.0
FN:Second Contact
EMAIL:second@example.com
END:VCARD
BEGIN:VCARD
VERSION:4.0
FN:Third Contact
EMAIL:third@example.com
END:VCARD")
    (let ((result (ecard-compat-parse-buffer)))
      (should (listp result))
      (should (= (length result) 3))
      (should (string= "First Contact" (ecard-get-property-value (nth 0 result) 'fn)))
      (should (string= "Second Contact" (ecard-get-property-value (nth 1 result) 'fn)))
      (should (string= "Third Contact" (ecard-get-property-value (nth 2 result) 'fn))))))

(ert-deftest ecard-compat-parse-buffer-empty ()
  "Test parsing empty buffer returns nil."
  (with-temp-buffer
    (let ((result (ecard-compat-parse-buffer)))
      (should (null result)))))

(ert-deftest ecard-compat-parse-buffer-different-versions ()
  "Test parsing different vCard versions from buffer."
  (with-temp-buffer
    (insert "BEGIN:VCARD
VERSION:2.1
FN:Version 2.1 Contact
TEL;HOME;VOICE:555-2100
EMAIL:v21@example.com
END:VCARD
BEGIN:VCARD
VERSION:3.0
FN:Version 3.0 Contact
TEL;TYPE=HOME,VOICE:555-3000
EMAIL;TYPE=INTERNET:v30@example.com
END:VCARD")
    (let ((result (ecard-compat-parse-buffer)))
      (should (listp result))
      (should (= (length result) 2))

      ;; Check first vCard (2.1)
      (let ((vc1 (nth 0 result)))
        (should (string= "Version 2.1 Contact" (ecard-get-property-value vc1 'fn)))
        (should (string= "v21@example.com" (ecard-get-property-value vc1 'email)))
        (should (string= "555-2100" (ecard-get-property-value vc1 'tel))))

      ;; Check second vCard (3.0)
      (let ((vc2 (nth 1 result)))
        (should (string= "Version 3.0 Contact" (ecard-get-property-value vc2 'fn)))
        (should (string= "v30@example.com" (ecard-get-property-value vc2 'email)))
        (should (string= "555-3000" (ecard-get-property-value vc2 'tel)))))))

(ert-deftest ecard-compat-parse-buffer-specific-buffer ()
  "Test parsing from specific buffer (not current buffer)."
  (let ((test-buffer (generate-new-buffer "*ecard-test*")))
    (unwind-protect
        (progn
          (with-current-buffer test-buffer
            (insert "BEGIN:VCARD
VERSION:4.0
FN:Buffer Test
EMAIL:buffer@example.com
END:VCARD"))
          ;; Parse from test-buffer while in a different buffer
          (with-temp-buffer
            (let ((result (ecard-compat-parse-buffer test-buffer)))
              (should (ecard-p result))
              (should (string= "Buffer Test" (ecard-get-property-value result 'fn)))
              (should (string= "buffer@example.com" (ecard-get-property-value result 'email))))))
      (kill-buffer test-buffer))))

;;; vCard 3.0 Serialization Tests
;;
;; Comprehensive tests for ecard-compat-serialize function
;; Tests RFC 2425 and RFC 2426 compliance

(ert-deftest ecard-compat-serialize-simple ()
  "Test basic vCard 3.0 serialization."
  (let* ((vc (ecard-create :fn "John Doe" :email "john@example.com"))
         (vcard-30 (ecard-compat-serialize vc)))
    (should (string-match-p "BEGIN:VCARD" vcard-30))
    (should (string-match-p "VERSION:3.0" vcard-30))
    (should (string-match-p "FN:John Doe" vcard-30))
    (should (string-match-p "EMAIL:john@example.com" vcard-30))
    (should (string-match-p "END:VCARD" vcard-30))))

(ert-deftest ecard-compat-serialize-structured-name ()
  "Test vCard 3.0 serialization with structured name (N property)."
  (let* ((vc (ecard-create
              :fn "John Quincy Doe Jr."
              :n '("Doe" "John" "Quincy" "Mr." "Jr.")))
         (vcard-30 (ecard-compat-serialize vc)))
    (should (string-match-p "N:Doe;John;Quincy;Mr\\.;Jr\\." vcard-30))))

(ert-deftest ecard-compat-serialize-tel-with-types ()
  "Test vCard 3.0 telephone serialization with TYPE parameters."
  (let* ((vc (ecard-create :fn "Jane Smith"))
         (tel-prop (ecard-property
                    :name "TEL"
                    :value "+1-555-1234"
                    :parameters '(("TYPE" . "home,voice")))))
    (oset vc tel (list tel-prop))
    (let ((vcard-30 (ecard-compat-serialize vc)))
      ;; TYPE values should be uppercased in vCard 3.0
      (should (string-match-p "TEL;TYPE=HOME,VOICE:\\+1-555-1234" vcard-30)))))

(ert-deftest ecard-compat-serialize-address ()
  "Test vCard 3.0 address serialization (ADR property)."
  (let* ((vc (ecard-create
              :fn "Jane Smith"
              :adr '("" "Suite 100" "123 Main St" "Springfield" "IL" "62701" "USA")))
         (vcard-30 (ecard-compat-serialize vc)))
    (should (string-match-p "ADR:;Suite 100;123 Main St;Springfield;IL;62701;USA" vcard-30))))

(ert-deftest ecard-compat-serialize-org ()
  "Test vCard 3.0 organization serialization (ORG property)."
  (let* ((vc (ecard-create
              :fn "Jane Smith"
              :org '("Example Corp" "Engineering" "R&D")))
         (vcard-30 (ecard-compat-serialize vc)))
    (should (string-match-p "ORG:Example Corp;Engineering;R&D" vcard-30))))

(ert-deftest ecard-compat-serialize-categories ()
  "Test vCard 3.0 categories serialization (comma-separated)."
  (let* ((vc (ecard-create
              :fn "Jane Smith"
              :categories '("Work" "Friend" "VIP")))
         (vcard-30 (ecard-compat-serialize vc)))
    (should (string-match-p "CATEGORIES:Work,Friend,VIP" vcard-30))))

(ert-deftest ecard-compat-serialize-nickname ()
  "Test vCard 3.0 nickname serialization (comma-separated)."
  (let* ((vc (ecard-create
              :fn "Jane Smith"
              :nickname '("Janie" "JayJay")))
         (vcard-30 (ecard-compat-serialize vc)))
    (should (string-match-p "NICKNAME:Janie,JayJay" vcard-30))))

(ert-deftest ecard-compat-serialize-non-ascii ()
  "Test vCard 3.0 serialization with non-ASCII characters.
Should add CHARSET=UTF-8 parameter."
  (let* ((vc (ecard-create :fn "José García"))
         (vcard-30 (ecard-compat-serialize vc)))
    ;; Should contain CHARSET=UTF-8 for non-ASCII
    (should (string-match-p "FN;CHARSET=UTF-8:José García" vcard-30))))

(ert-deftest ecard-compat-serialize-photo-data-uri ()
  "Test vCard 3.0 serialization with PHOTO as data URI.
Should convert to ENCODING=BASE64."
  (let* ((vc (ecard-create
              :fn "Jane Smith"
              :photo "data:image/jpeg;base64,/9j/4AAQSkZJRg=="))
         (vcard-30 (ecard-compat-serialize vc)))
    ;; Should extract and use BASE64 encoding
    ;; Note: TYPE comes before ENCODING due to parameter ordering
    (should (string-match-p "PHOTO;TYPE=IMAGE/JPEG;ENCODING=BASE64:/9j/4AAQSkZJRg==" vcard-30))))

(ert-deftest ecard-compat-serialize-logo-data-uri ()
  "Test vCard 3.0 serialization with LOGO as data URI."
  (let* ((vc (ecard-create
              :fn "Company Name"
              :logo "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgA="))
         (vcard-30 (ecard-compat-serialize vc)))
    ;; Note: TYPE comes before ENCODING due to parameter ordering
    (should (string-match-p "LOGO;TYPE=IMAGE/PNG;ENCODING=BASE64:iVBORw0KGgoAAAANSUhEUgA=" vcard-30))))

(ert-deftest ecard-compat-serialize-uid ()
  "Test vCard 3.0 serialization with UID.
urn:uuid: prefix is preserved (optional in 3.0, required in 4.0)."
  (let* ((vc (ecard-create
              :fn "Jane Smith"
              :uid "urn:uuid:12345678-1234-1234-1234-123456789abc"))
         (vcard-30 (ecard-compat-serialize vc)))
    (should (string-match-p "UID:urn:uuid:12345678-1234-1234-1234-123456789abc" vcard-30))))

(ert-deftest ecard-compat-serialize-extended-properties ()
  "Test vCard 3.0 serialization with X-* extended properties."
  (let* ((vc (ecard-create :fn "Jane Smith"))
         (x-prop (ecard-property
                  :name "X-CUSTOM"
                  :value "custom-value")))
    (oset vc extended (list (cons "X-CUSTOM" (list x-prop))))
    (let ((vcard-30 (ecard-compat-serialize vc)))
      (should (string-match-p "X-CUSTOM:custom-value" vcard-30)))))

(ert-deftest ecard-compat-serialize-group ()
  "Test vCard 3.0 serialization with grouped properties."
  (let* ((vc (ecard-create :fn "Jane Smith"))
         (email-prop (ecard-property
                      :name "EMAIL"
                      :value "jane@example.com"
                      :group "WORK")))
    (oset vc email (list email-prop))
    (let ((vcard-30 (ecard-compat-serialize vc)))
      (should (string-match-p "WORK\\.EMAIL:jane@example.com" vcard-30)))))

(ert-deftest ecard-compat-serialize-multiple ()
  "Test serialization of multiple vCards to vCard 3.0."
  (let* ((vc1 (ecard-create :fn "John Doe" :email "john@example.com"))
         (vc2 (ecard-create :fn "Jane Smith" :email "jane@example.com"))
         (vcards-30 (ecard-compat-serialize-multiple (list vc1 vc2))))
    ;; Should contain two vCards
    (should (= (length (split-string vcards-30 "BEGIN:VCARD")) 3))
    (should (string-match-p "FN:John Doe" vcards-30))
    (should (string-match-p "FN:Jane Smith" vcards-30))
    ;; Check for two VERSION:3.0 occurrences (one per vCard)
    (should (= (length (split-string vcards-30 "VERSION:3.0")) 3))))

(ert-deftest ecard-compat-serialize-round-trip-basic ()
  "Test round-trip conversion: 3.0 → 4.0 → 3.0."
  (let* ((vcard-30-original "BEGIN:VCARD
VERSION:3.0
FN:John Doe
EMAIL;TYPE=HOME:john@example.com
TEL;TYPE=WORK,VOICE:+1-555-1234
END:VCARD")
         ;; Parse 3.0 to 4.0 object
         (vc (ecard-compat-parse-30 vcard-30-original))
         ;; Serialize back to 3.0
         (vcard-30-new (ecard-compat-serialize vc)))
    ;; Verify essential properties preserved
    (should (string-match-p "VERSION:3.0" vcard-30-new))
    (should (string-match-p "FN:John Doe" vcard-30-new))
    (should (string-match-p "EMAIL.*john@example.com" vcard-30-new))
    (should (string-match-p "TEL.*\\+1-555-1234" vcard-30-new))))

(ert-deftest ecard-compat-serialize-bday ()
  "Test vCard 3.0 serialization with birthday."
  (let* ((vc (ecard-create :fn "Jane Smith" :bday "19850615"))
         (vcard-30 (ecard-compat-serialize vc)))
    (should (string-match-p "BDAY:19850615" vcard-30))))

(ert-deftest ecard-compat-serialize-anniversary ()
  "Test vCard 3.0 serialization with anniversary."
  (let* ((vc (ecard-create :fn "Jane Smith" :anniversary "20100701"))
         (vcard-30 (ecard-compat-serialize vc)))
    (should (string-match-p "ANNIVERSARY:20100701" vcard-30))))

(ert-deftest ecard-compat-serialize-gender ()
  "Test vCard 3.0 serialization with gender."
  (let* ((vc (ecard-create :fn "Jane Smith" :gender '("F" "Female")))
         (vcard-30 (ecard-compat-serialize vc)))
    (should (string-match-p "GENDER:F;Female" vcard-30))))

(ert-deftest ecard-compat-serialize-geo ()
  "Test vCard 3.0 serialization with geographical position."
  (let* ((vc (ecard-create :fn "Jane Smith" :geo "geo:37.386013,-122.082932"))
         (vcard-30 (ecard-compat-serialize vc)))
    ;; Comma in value is escaped per vCard escaping rules
    (should (string-match-p "GEO:geo:37\\.386013\\\\,-122\\.082932" vcard-30))))

(ert-deftest ecard-compat-serialize-title-role ()
  "Test vCard 3.0 serialization with title and role."
  (let* ((vc (ecard-create
              :fn "Jane Smith"
              :title "Senior Engineer"
              :role "Team Lead"))
         (vcard-30 (ecard-compat-serialize vc)))
    (should (string-match-p "TITLE:Senior Engineer" vcard-30))
    (should (string-match-p "ROLE:Team Lead" vcard-30))))

(ert-deftest ecard-compat-serialize-note ()
  "Test vCard 3.0 serialization with note."
  (let* ((vc (ecard-create
              :fn "Jane Smith"
              :note "Important contact"))
         (vcard-30 (ecard-compat-serialize vc)))
    (should (string-match-p "NOTE:Important contact" vcard-30))))

(ert-deftest ecard-compat-serialize-url ()
  "Test vCard 3.0 serialization with URL."
  (let* ((vc (ecard-create
              :fn "Jane Smith"
              :url "https://example.com"))
         (vcard-30 (ecard-compat-serialize vc)))
    (should (string-match-p "URL:https://example.com" vcard-30))))

(ert-deftest ecard-compat-serialize-multiple-emails ()
  "Test vCard 3.0 serialization with multiple email addresses."
  (let* ((vc (ecard-create :fn "Jane Smith"))
         (email1 (ecard-property
                  :name "EMAIL"
                  :value "jane@example.com"
                  :parameters '(("TYPE" . "work"))))
         (email2 (ecard-property
                  :name "EMAIL"
                  :value "jane.personal@example.org"
                  :parameters '(("TYPE" . "home")))))
    (oset vc email (list email1 email2))
    (let ((vcard-30 (ecard-compat-serialize vc)))
      (should (string-match-p "EMAIL;TYPE=WORK:jane@example.com" vcard-30))
      (should (string-match-p "EMAIL;TYPE=HOME:jane.personal@example.org" vcard-30)))))

(ert-deftest ecard-compat-serialize-multiple-phones ()
  "Test vCard 3.0 serialization with multiple phone numbers."
  (let* ((vc (ecard-create :fn "Jane Smith"))
         (tel1 (ecard-property
                :name "TEL"
                :value "+1-555-1234"
                :parameters '(("TYPE" . "work,voice"))))
         (tel2 (ecard-property
                :name "TEL"
                :value "+1-555-5678"
                :parameters '(("TYPE" . "home,voice"))))
         (tel3 (ecard-property
                :name "TEL"
                :value "+1-555-9999"
                :parameters '(("TYPE" . "cell")))))
    (oset vc tel (list tel1 tel2 tel3))
    (let ((vcard-30 (ecard-compat-serialize vc)))
      (should (string-match-p "TEL;TYPE=WORK,VOICE:\\+1-555-1234" vcard-30))
      (should (string-match-p "TEL;TYPE=HOME,VOICE:\\+1-555-5678" vcard-30))
      (should (string-match-p "TEL;TYPE=CELL:\\+1-555-9999" vcard-30)))))

(ert-deftest ecard-compat-serialize-escaping ()
  "Test vCard 3.0 serialization with special characters requiring escaping."
  (let* ((vc (ecard-create
              :fn "Test, User"
              :note "Line 1\nLine 2\nLine 3"))
         (vcard-30 (ecard-compat-serialize vc)))
    ;; Comma should be escaped
    (should (string-match-p "FN:Test\\\\, User" vcard-30))
    ;; Newlines should be escaped
    (should (string-match-p "NOTE:Line 1\\\\nLine 2\\\\nLine 3" vcard-30))))

(ert-deftest ecard-compat-serialize-complex-vcard ()
  "Test vCard 3.0 serialization with many properties (comprehensive test)."
  (let* ((vc (ecard-create
              :fn "Dr. Jane Quincy Smith Jr."
              :n '("Smith" "Jane" "Quincy" "Dr." "Jr.")
              :nickname '("JQ" "Janie")
              :email "jane@example.com"
              :tel "+1-555-1234"
              :adr '("" "Suite 200" "456 Oak Ave" "Springfield" "IL" "62701" "USA")
              :org '("Example Corp" "Engineering")
              :title "Chief Engineer"
              :role "Technical Lead"
              :categories '("Work" "VIP")
              :note "Important contact - handles all technical decisions"
              :url "https://jane.example.com"
              :bday "19800115"
              :uid "urn:uuid:abcd-1234-efgh-5678"))
         (vcard-30 (ecard-compat-serialize vc)))
    ;; Verify all properties present
    (should (string-match-p "VERSION:3.0" vcard-30))
    (should (string-match-p "FN:Dr\\. Jane Quincy Smith Jr\\." vcard-30))
    (should (string-match-p "N:Smith;Jane;Quincy;Dr\\.;Jr\\." vcard-30))
    (should (string-match-p "NICKNAME:JQ,Janie" vcard-30))
    (should (string-match-p "EMAIL:jane@example.com" vcard-30))
    (should (string-match-p "TEL:\\+1-555-1234" vcard-30))
    (should (string-match-p "ADR:;Suite 200;456 Oak Ave;Springfield;IL;62701;USA" vcard-30))
    (should (string-match-p "ORG:Example Corp;Engineering" vcard-30))
    (should (string-match-p "TITLE:Chief Engineer" vcard-30))
    (should (string-match-p "ROLE:Technical Lead" vcard-30))
    (should (string-match-p "CATEGORIES:Work,VIP" vcard-30))
    (should (string-match-p "NOTE:Important contact" vcard-30))
    (should (string-match-p "URL:https://jane\\.example\\.com" vcard-30))
    (should (string-match-p "BDAY:19800115" vcard-30))
    (should (string-match-p "UID:urn:uuid:abcd-1234-efgh-5678" vcard-30))))

;;; Regression tests for bug fixes

(ert-deftest ecard-compat-regression-no-colon-error ()
  "Regression test for 'No value separator colon found' error.
This error occurred when parsing vCard 3.0 data from Radicale CardDAV
server that had malformed property lines without colon separators.

The fix ensures that:
1. ecard--find-value-separator returns nil instead of throwing error
2. ecard-compat--parse-legacy-property logs warnings for unparseable lines
3. Parsing continues even with malformed properties"
  ;; Test vCard with missing colon in property line
  (let ((vcard-text "BEGIN:VCARD
VERSION:3.0
FN:Test User
N:User;Test;;;
EMAIL;TYPE=HOME:test@example.com
TEL;TYPE=HOME
NOTE:This vCard has a malformed TEL property
END:VCARD"))
    ;; Should parse without error
    (let ((card (ecard-compat-parse vcard-text)))
      (should card)
      (should (string= (ecard-get-property-value card 'fn) "Test User"))
      ;; TEL property should be skipped due to malformed line
      (should (null (ecard-tel card)))
      ;; Other properties should parse correctly
      (should (ecard-email card))
      (should (string= (ecard-get-property-value card 'email) "test@example.com")))))

(ert-deftest ecard-compat-regression-find-value-separator-no-colons ()
  "Test that ecard--find-value-separator returns nil when no colons found.
This is a regression test for the bug where the function threw an error
instead of returning nil, preventing proper error handling."
  ;; Test with no colons at all
  (should (null (ecard--find-value-separator "TYPE=HOME,WORK")))
  ;; Test with empty string
  (should (null (ecard--find-value-separator "")))
  ;; Test with only quoted content (colons in quotes don't count)
  (should (null (ecard--find-value-separator "TYPE=\"test:value\""))))

(ert-deftest ecard-compat-regression-find-value-separator-with-colons ()
  "Test that ecard--find-value-separator still works correctly with colons.
Ensures the fix didn't break existing functionality."
  ;; Simple case: one colon
  (should (= (ecard--find-value-separator "TYPE=HOME:john@example.com") 9))
  ;; Multiple colons in value (URI)
  (should (= (ecard--find-value-separator "TYPE=work:urn:uuid:12345") 9))
  ;; No parameters, just colon
  (let ((pos (ecard--find-value-separator ":value")))
    (should (= pos 0))))

;;; Encoding dispatch tests

(ert-deftest ecard-compat-decode-value-base64 ()
  "Test decode-value dispatching to BASE64 decoder."
  (should (string= (ecard-compat--decode-value "SGVsbG8=" "BASE64")
                   "Hello"))
  ;; Also works with lowercase "b" encoding label
  (should (string= (ecard-compat--decode-value "SGVsbG8=" "b")
                   "Hello")))

(ert-deftest ecard-compat-decode-value-quoted-printable ()
  "Test decode-value dispatching to QUOTED-PRINTABLE decoder."
  (should (string= (ecard-compat--decode-value "Hello=20World" "QUOTED-PRINTABLE")
                   "Hello World"))
  ;; Also works with "Q" encoding label
  (should (string= (ecard-compat--decode-value "Hello=20World" "Q")
                   "Hello World")))

(ert-deftest ecard-compat-decode-value-unknown-encoding ()
  "Test decode-value with unknown encoding returns value as-is."
  (should (string= (ecard-compat--decode-value "test" "UNKNOWN")
                   "test")))

(ert-deftest ecard-compat-decode-value-nil-encoding ()
  "Test decode-value with nil encoding returns nil."
  (should (null (ecard-compat--decode-value "test" nil))))

(ert-deftest ecard-compat-decode-value-base64-with-whitespace ()
  "Test decode-value strips whitespace from BASE64 data."
  (should (string= (ecard-compat--decode-value "SGVs\nbG8=" "BASE64")
                   "Hello")))

;;; Quoted-printable with charset

(ert-deftest ecard-compat-decode-quoted-printable-with-charset ()
  "Test QUOTED-PRINTABLE decoding with charset conversion."
  (let ((result (ecard-compat--decode-quoted-printable "Hello" "UTF-8")))
    (should (string= result "Hello"))))

;;; Charset conversion edge cases

(ert-deftest ecard-compat-convert-charset-iso-8859-1 ()
  "Test charset conversion from ISO-8859-1."
  (let* ((raw (unibyte-string #xe9))  ; é in ISO-8859-1
         (result (ecard-compat--convert-charset raw "ISO-8859-1")))
    (should (string= result "\u00e9"))))

(ert-deftest ecard-compat-convert-charset-unknown ()
  "Test charset conversion with unknown charset returns value as-is."
  (should (string= (ecard-compat--convert-charset "Test" "UNKNOWN-CHARSET-XYZ")
                   "Test")))

;;; Parse legacy params tests

(ert-deftest ecard-compat-parse-legacy-params-key-value ()
  "Test parsing legacy params with key=value format."
  (let ((result (ecard-compat--parse-legacy-params "TYPE=HOME;CHARSET=UTF-8" 'v30)))
    (should (assoc "TYPE" result))
    (should (equal (cdr (assoc "TYPE" result)) "HOME"))
    (should (equal (cdr (assoc "CHARSET" result)) "UTF-8"))))

(ert-deftest ecard-compat-parse-legacy-params-bare-types ()
  "Test parsing legacy params with bare type indicators (vCard 2.1 style)."
  (let ((result (ecard-compat--parse-legacy-params "HOME;VOICE;FAX" 'v21)))
    (should (assoc "HOME" result))
    (should (eq (cdr (assoc "HOME" result)) t))
    (should (eq (cdr (assoc "VOICE" result)) t))))

(ert-deftest ecard-compat-parse-legacy-params-quoted-value ()
  "Test parsing legacy params with quoted values."
  (let ((result (ecard-compat--parse-legacy-params "TYPE=\"HOME,WORK\"" 'v30)))
    (should (equal (cdr (assoc "TYPE" result)) "HOME,WORK"))))

(ert-deftest ecard-compat-parse-legacy-params-nil ()
  "Test parsing nil param string returns nil."
  (should (null (ecard-compat--parse-legacy-params nil 'v30)))
  (should (null (ecard-compat--parse-legacy-params "" 'v30))))

;;; Parse legacy property tests

(ert-deftest ecard-compat-parse-legacy-property-simple ()
  "Test parsing simple legacy property line."
  (let ((result (ecard-compat--parse-legacy-property "FN:John Doe" 'v30)))
    (should result)
    (should (equal (plist-get result :name) "FN"))
    (should (equal (plist-get result :value) "John Doe"))))

(ert-deftest ecard-compat-parse-legacy-property-with-params ()
  "Test parsing legacy property with parameters."
  (let ((result (ecard-compat--parse-legacy-property
                 "TEL;TYPE=HOME,VOICE:555-1234" 'v30)))
    (should result)
    (should (equal (plist-get result :name) "TEL"))
    (should (equal (plist-get result :value) "555-1234"))
    (let ((params (plist-get result :params)))
      (should (assoc "TYPE" params)))))

(ert-deftest ecard-compat-parse-legacy-property-with-group ()
  "Test parsing legacy property with group prefix."
  (let ((result (ecard-compat--parse-legacy-property
                 "item1.TEL:555-1234" 'v30)))
    (should result)
    (should (equal (plist-get result :group) "item1"))
    (should (equal (plist-get result :name) "TEL"))))

(ert-deftest ecard-compat-parse-legacy-property-binary-skip ()
  "Test parsing skips lines with binary/control characters."
  (let ((result (ecard-compat--parse-legacy-property
                 (concat "PHOTO:" (string 0) "binary") 'v21)))
    (should (null result))))

(ert-deftest ecard-compat-parse-legacy-property-21-encoding ()
  "Test parsing vCard 2.1 property with ENCODING parameter."
  (let ((result (ecard-compat--parse-legacy-property
                 "NOTE;ENCODING=QUOTED-PRINTABLE:Hello=20World" 'v21)))
    (should result)
    (should (equal (plist-get result :encoding) "QUOTED-PRINTABLE"))))

(ert-deftest ecard-compat-parse-legacy-property-21-charset ()
  "Test parsing vCard 2.1 property with CHARSET parameter."
  (let ((result (ecard-compat--parse-legacy-property
                 "NOTE;CHARSET=ISO-8859-1:test" 'v21)))
    (should result)
    (should (equal (plist-get result :charset) "ISO-8859-1"))))

;;; Add property to ecard tests

(ert-deftest ecard-compat-add-property-single ()
  "Test adding a single property to ecard."
  (let ((vc (ecard-create :fn "Test")))
    (ecard-compat--add-property-to-ecard vc "EMAIL" "test@example.com" nil nil)
    (should (= (length (ecard-email vc)) 1))
    (should (equal (ecard-property-value (car (ecard-email vc)))
                   "test@example.com"))))

(ert-deftest ecard-compat-add-property-multiple ()
  "Test adding multiple properties to ecard."
  (let ((vc (ecard-create :fn "Test")))
    (ecard-compat--add-property-to-ecard vc "EMAIL" "first@example.com" nil nil)
    (ecard-compat--add-property-to-ecard vc "EMAIL" "second@example.com" nil nil)
    (should (= (length (ecard-email vc)) 2))))

(ert-deftest ecard-compat-add-property-cardinality-one ()
  "Test that cardinality-one properties are replaced, not appended."
  (let ((vc (ecard-create :fn "Original")))
    ;; FN is cardinality *1 in RFC 6350, but actually it's 1*
    ;; Test with a true cardinality-one property like KIND
    (ecard-compat--add-property-to-ecard vc "KIND" "individual" nil nil)
    (ecard-compat--add-property-to-ecard vc "KIND" "organization" nil nil)
    ;; Should have only the replacement
    (should (= (length (ecard-kind vc)) 1))))

(ert-deftest ecard-compat-add-property-with-group ()
  "Test adding property with group prefix."
  (let ((vc (ecard-create :fn "Test")))
    (ecard-compat--add-property-to-ecard vc "EMAIL" "test@example.com"
                                          '(("TYPE" . "work")) "item1")
    (let ((prop (car (ecard-email vc))))
      (should (equal (ecard-property-group prop) "item1")))))

(ert-deftest ecard-compat-add-property-skips-version ()
  "Test that VERSION property is not overridden."
  (let ((vc (ecard-create :fn "Test")))
    (ecard-compat--add-property-to-ecard vc "VERSION" "3.0" nil nil)
    ;; VERSION should still be 4.0 from ecard-create
    (should (equal (ecard-get-property-value vc 'version) "4.0"))))

;;; Build ecard 4.0 tests

(ert-deftest ecard-compat-build-ecard-40-basic ()
  "Test building vCard 4.0 object from properties."
  (let ((props (list (list :name "EMAIL" :value "test@example.com"
                           :params nil :group nil)
                     (list :name "TEL" :value "+1-555-1234"
                           :params '(("TYPE" . "home")) :group nil))))
    (let ((vc (ecard-compat--build-ecard-40 "John Doe" props)))
      (should (ecard-p vc))
      (should (equal (ecard-get-property-value vc 'fn) "John Doe"))
      (should (equal (ecard-get-property-value vc 'email) "test@example.com"))
      (should (equal (ecard-get-property-value vc 'tel) "+1-555-1234")))))

(ert-deftest ecard-compat-build-ecard-40-nil-fn ()
  "Test building vCard 4.0 with nil FN uses 'Unknown'."
  (let ((vc (ecard-compat--build-ecard-40 nil nil)))
    (should (equal (ecard-get-property-value vc 'fn) "Unknown"))))

;;; Process property value tests

(ert-deftest ecard-compat-process-property-value-structured ()
  "Test processing structured values (N, ADR)."
  (let ((result (ecard-compat--process-property-value
                 "N" "Doe;John;Q;Mr;Jr" nil nil nil 'v30)))
    (should (listp result))
    (should (equal (car result) "Doe"))
    (should (equal (cadr result) "John"))))

(ert-deftest ecard-compat-process-property-value-text-list ()
  "Test processing text-list values (CATEGORIES)."
  (let ((result (ecard-compat--process-property-value
                 "CATEGORIES" "Work,Friend,VIP" nil nil nil 'v30)))
    (should (listp result))
    (should (equal result '("Work" "Friend" "VIP")))))

(ert-deftest ecard-compat-process-property-value-encoded-base64 ()
  "Test processing BASE64-encoded binary property."
  (let ((result (ecard-compat--process-property-value
                 "PHOTO" "SGVsbG8=" nil "BASE64" nil 'v21)))
    ;; Should be a data URI
    (should (string-prefix-p "data:image/jpeg;base64," result))))

(ert-deftest ecard-compat-process-property-value-with-charset ()
  "Test processing value with charset but no encoding."
  (let ((result (ecard-compat--process-property-value
                 "NOTE" "Hello" nil nil "UTF-8" 'v21)))
    (should (string= result "Hello"))))

;;; Media type detection tests

(ert-deftest ecard-compat-detect-media-type-logo ()
  "Test media type detection for LOGO."
  (should (string= (ecard-compat--detect-media-type "LOGO" nil)
                   "image/png")))

(ert-deftest ecard-compat-detect-media-type-sound ()
  "Test media type detection for SOUND."
  (should (string= (ecard-compat--detect-media-type "SOUND" nil)
                   "audio/basic")))

(ert-deftest ecard-compat-detect-media-type-uri ()
  "Test media type detection returns nil for VALUE=uri."
  (should (null (ecard-compat--detect-media-type
                 "PHOTO" '(("VALUE" . "uri"))))))

(ert-deftest ecard-compat-detect-media-type-unknown ()
  "Test media type detection for unknown property."
  (should (null (ecard-compat--detect-media-type "NOTE" nil))))

;;; Data URI extraction tests

(ert-deftest ecard-compat-extract-data-uri ()
  "Test data URI extraction."
  (let ((result (ecard-compat--extract-data-uri
                 "data:image/jpeg;base64,/9j/4AAQ")))
    (should result)
    (should (equal (car result) "image/jpeg"))
    (should (equal (cdr result) "/9j/4AAQ"))))

(ert-deftest ecard-compat-extract-data-uri-nil ()
  "Test data URI extraction with nil input."
  (should (null (ecard-compat--extract-data-uri nil))))

(ert-deftest ecard-compat-extract-data-uri-non-data ()
  "Test data URI extraction with non-data URI."
  (should (null (ecard-compat--extract-data-uri "https://example.com/photo.jpg"))))

(ert-deftest ecard-compat-extract-data-uri-malformed ()
  "Test data URI extraction with malformed data URI."
  (should (null (ecard-compat--extract-data-uri "data:broken"))))

;;; Quoted-printable encoding tests

(ert-deftest ecard-compat-needs-quoted-printable-p ()
  "Test detection of non-ASCII characters."
  (should-not (ecard-compat--needs-quoted-printable-p "Hello"))
  (should (ecard-compat--needs-quoted-printable-p "José"))
  (should-not (ecard-compat--needs-quoted-printable-p nil))
  (should-not (ecard-compat--needs-quoted-printable-p ""))
  (should-not (ecard-compat--needs-quoted-printable-p 42)))

(ert-deftest ecard-compat-encode-quoted-printable ()
  "Test QUOTED-PRINTABLE encoding."
  (should (string= (ecard-compat--encode-quoted-printable "Hello")
                   "Hello"))
  ;; Space (32) should be encoded since < 33
  (should (string= (ecard-compat--encode-quoted-printable "A B")
                   "A=20B"))
  ;; = sign should be encoded
  (should (string= (ecard-compat--encode-quoted-printable "A=B")
                   "A=3DB")))

(ert-deftest ecard-compat-encode-quoted-printable-non-ascii ()
  "Test QUOTED-PRINTABLE encoding of non-ASCII characters."
  (let ((result (ecard-compat--encode-quoted-printable "é")))
    ;; UTF-8 encoding of é is C3 A9
    (should (string= result "=C3=A9"))))

;;; Format parameters 3.0 tests

(ert-deftest ecard-compat-format-parameters-30-empty ()
  "Test formatting empty parameters."
  (should (string= (ecard-compat--format-parameters-30 nil) "")))

(ert-deftest ecard-compat-format-parameters-30-key-value ()
  "Test formatting key=value parameters."
  (let ((result (ecard-compat--format-parameters-30
                 '(("TYPE" . "HOME") ("CHARSET" . "UTF-8")))))
    (should (string= result "TYPE=HOME;CHARSET=UTF-8"))))

(ert-deftest ecard-compat-format-parameters-30-bare ()
  "Test formatting bare parameters (value is t)."
  (let ((result (ecard-compat--format-parameters-30
                 '(("HOME" . t)))))
    (should (string= result "HOME"))))

;;; Convert params to 3.0 tests

(ert-deftest ecard-compat-convert-params-to-30-type ()
  "Test TYPE parameter conversion to vCard 3.0 (uppercased)."
  (let ((result (ecard-compat--convert-params-to-30
                 '(("TYPE" . "home,work")) "TEL" "+1-555")))
    (should (assoc "TYPE" result))
    (should (string= (cdr (assoc "TYPE" result)) "HOME,WORK"))))

(ert-deftest ecard-compat-convert-params-to-30-value ()
  "Test VALUE parameter conversion to vCard 3.0."
  (let ((result (ecard-compat--convert-params-to-30
                 '(("VALUE" . "uri")) "PHOTO" "https://example.com")))
    (should (assoc "VALUE" result))
    (should (string= (cdr (assoc "VALUE" result)) "URI"))))

(ert-deftest ecard-compat-convert-params-to-30-non-ascii ()
  "Test CHARSET=UTF-8 added for non-ASCII text."
  (let ((result (ecard-compat--convert-params-to-30
                 nil "FN" "José García")))
    (should (assoc "CHARSET" result))
    (should (string= (cdr (assoc "CHARSET" result)) "UTF-8"))))

(ert-deftest ecard-compat-convert-params-to-30-binary-no-charset ()
  "Test CHARSET not added for binary properties."
  (let ((result (ecard-compat--convert-params-to-30
                 nil "PHOTO" "binary-data-with-non-ascii")))
    ;; PHOTO is binary, should not get CHARSET
    (should (null (assoc "CHARSET" result)))))

;;; Format property 3.0 tests

(ert-deftest ecard-compat-format-property-30-simple ()
  "Test simple property formatting for vCard 3.0."
  (let ((prop (ecard-property :name "FN" :value "John Doe")))
    (let ((result (ecard-compat--format-property-30 prop)))
      (should (string= result "FN:John Doe")))))

(ert-deftest ecard-compat-format-property-30-with-type ()
  "Test property formatting with TYPE parameter."
  (let ((prop (ecard-property :name "TEL"
                              :value "+1-555-1234"
                              :parameters '(("TYPE" . "home,voice")))))
    (let ((result (ecard-compat--format-property-30 prop)))
      (should (string-match-p "TEL;TYPE=HOME,VOICE:\\+1-555-1234" result)))))

(ert-deftest ecard-compat-format-property-30-with-group ()
  "Test property formatting with group prefix."
  (let ((prop (ecard-property :name "EMAIL"
                              :value "test@example.com"
                              :group "item1")))
    (let ((result (ecard-compat--format-property-30 prop)))
      (should (string-match-p "^item1\\.EMAIL:test@example\\.com$" result)))))

(ert-deftest ecard-compat-format-property-30-structured ()
  "Test property formatting with structured value (semicolons)."
  (let ((prop (ecard-property :name "N"
                              :value '("Doe" "John" "Q" "Mr" "Jr"))))
    (let ((result (ecard-compat--format-property-30 prop)))
      (should (string-match-p "N:Doe;John;Q;Mr;Jr" result)))))

(ert-deftest ecard-compat-format-property-30-categories ()
  "Test property formatting with text-list value (commas)."
  (let ((prop (ecard-property :name "CATEGORIES"
                              :value '("Work" "Friend"))))
    (let ((result (ecard-compat--format-property-30 prop)))
      (should (string-match-p "CATEGORIES:Work,Friend" result)))))

(ert-deftest ecard-compat-format-property-30-data-uri-photo ()
  "Test property formatting converts data URI to BASE64 for binary."
  (let ((prop (ecard-property :name "PHOTO"
                              :value "data:image/jpeg;base64,/9j/4AAQ")))
    (let ((result (ecard-compat--format-property-30 prop)))
      (should (string-match-p "ENCODING=BASE64" result))
      (should (string-match-p "/9j/4AAQ" result)))))

;;; Serialize properties 3.0 tests

(ert-deftest ecard-compat-serialize-properties-30-single ()
  "Test serializing a single property."
  (let* ((props (list (ecard-property :name "EMAIL" :value "test@example.com")))
         (lines (ecard-compat--serialize-properties-30 props)))
    (should (> (length lines) 0))
    (should (cl-some (lambda (l) (string-match-p "EMAIL:test@example.com" l)) lines))))

(ert-deftest ecard-compat-serialize-properties-30-text-list-combine ()
  "Test serializing multiple text-list properties combines them."
  (let* ((props (list (ecard-property :name "CATEGORIES" :value "Work")
                      (ecard-property :name "CATEGORIES" :value "VIP")))
         (lines (ecard-compat--serialize-properties-30 props)))
    ;; Should combine into one CATEGORIES line
    (should (cl-some (lambda (l) (string-match-p "CATEGORIES:Work,VIP" l)) lines))))

(ert-deftest ecard-compat-serialize-properties-30-text-list-single ()
  "Test serializing single text-list property with list value."
  (let* ((props (list (ecard-property :name "CATEGORIES"
                                      :value '("A" "B" "C"))))
         (lines (ecard-compat--serialize-properties-30 props)))
    (should (cl-some (lambda (l) (string-match-p "CATEGORIES:A,B,C" l)) lines))))

;;; Parse file tests

(ert-deftest ecard-compat-parse-file-single ()
  "Test parsing a single vCard from file."
  (let ((temp-file (make-temp-file "ecard-test-" nil ".vcf")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "BEGIN:VCARD\n")
            (insert "VERSION:3.0\n")
            (insert "FN:File Test\n")
            (insert "EMAIL:file@example.com\n")
            (insert "END:VCARD\n"))
          (let ((result (ecard-compat-parse-file temp-file)))
            (should (ecard-p result))
            (should (equal (ecard-get-property-value result 'fn) "File Test"))))
      (delete-file temp-file))))

(ert-deftest ecard-compat-parse-file-multiple ()
  "Test parsing multiple vCards from file."
  (let ((temp-file (make-temp-file "ecard-test-" nil ".vcf")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "BEGIN:VCARD\nVERSION:3.0\nFN:First\nEND:VCARD\n")
            (insert "BEGIN:VCARD\nVERSION:3.0\nFN:Second\nEND:VCARD\n"))
          (let ((result (ecard-compat-parse-file temp-file)))
            (should (listp result))
            (should (= (length result) 2))))
      (delete-file temp-file))))

;;; vCard 3.0 parameter conversion tests (additional)

(ert-deftest ecard-compat-convert-params-30-encoding ()
  "Test vCard 3.0 ENCODING parameter extraction."
  (let* ((params '(("ENCODING" . "b")))
         (result (ecard-compat--convert-params-30 params "PHOTO")))
    (should (string= (plist-get result :encoding) "b"))))

(ert-deftest ecard-compat-convert-params-30-charset ()
  "Test vCard 3.0 CHARSET parameter extraction."
  (let* ((params '(("CHARSET" . "ISO-8859-1")))
         (result (ecard-compat--convert-params-30 params "NOTE")))
    (should (string= (plist-get result :charset) "ISO-8859-1"))))

(ert-deftest ecard-compat-convert-params-30-value-lowercase ()
  "Test vCard 3.0 VALUE parameter lowercased."
  (let* ((params '(("VALUE" . "URI")))
         (result (ecard-compat--convert-params-30 params "PHOTO"))
         (converted (plist-get result :params)))
    (should (equal (cdr (assoc "VALUE" converted)) "uri"))))

(ert-deftest ecard-compat-convert-params-30-drops-internet ()
  "Test vCard 3.0 drops INTERNET type value."
  (let* ((params '(("TYPE" . "INTERNET")))
         (result (ecard-compat--convert-params-30 params "EMAIL"))
         (converted (plist-get result :params)))
    ;; INTERNET maps to nil, so TYPE should be dropped
    (should (null (assoc "TYPE" converted)))))

;;; vCard 2.1 parameter conversion tests (additional)

(ert-deftest ecard-compat-convert-params-21-value ()
  "Test vCard 2.1 VALUE parameter conversion."
  (let* ((params '(("VALUE" . "URI")))
         (result (ecard-compat--convert-params-21 params "PHOTO"))
         (converted (plist-get result :params)))
    (should (equal (cdr (assoc "VALUE" converted)) "uri"))))

(ert-deftest ecard-compat-convert-params-21-pref ()
  "Test vCard 2.1 PREF type indicator conversion."
  (let* ((params '(("PREF" . t) ("HOME" . t)))
         (result (ecard-compat--convert-params-21 params "TEL"))
         (converted (plist-get result :params)))
    (should (assoc "TYPE" converted))
    (should (string-match-p "pref" (cdr (assoc "TYPE" converted))))
    (should (string-match-p "home" (cdr (assoc "TYPE" converted))))))

(ert-deftest ecard-compat-convert-params-21-other-params ()
  "Test vCard 2.1 passes through unknown parameters."
  (let* ((params '(("X-CUSTOM" . "value")))
         (result (ecard-compat--convert-params-21 params "TEL"))
         (converted (plist-get result :params)))
    (should (assoc "X-CUSTOM" converted))))

;;; Property should-include tests (additional)

(ert-deftest ecard-compat-should-include-property-agent ()
  "Test AGENT property is dropped."
  (should-not (ecard-compat--should-include-property-p "AGENT")))

(ert-deftest ecard-compat-should-include-property-name ()
  "Test NAME property is dropped."
  (should-not (ecard-compat--should-include-property-p "NAME")))

(ert-deftest ecard-compat-should-include-property-x-custom ()
  "Test X-* properties are included."
  (should (ecard-compat--should-include-property-p "X-CUSTOM")))

;;; vCard 4.0 fallback to 3.0 parser tests

(ert-deftest ecard-compat-parse-40-malformed-fallback ()
  "Test that malformed vCard 4.0 falls back to legacy parser."
  ;; A vCard 4.0 with a property line that might cause strict parser to fail
  ;; but legacy parser can handle
  (let* ((ecard-40 "BEGIN:VCARD
VERSION:4.0
FN:Fallback Test
TEL:555-1234
END:VCARD")
         (vc (ecard-compat-parse ecard-40)))
    (should (ecard-p vc))
    (should (string= (ecard-get-property-value vc 'fn) "Fallback Test"))))

;;; vCard 2.1 with multiple encodings

(ert-deftest ecard-compat-parse-21-mixed-encodings ()
  "Test parsing vCard 2.1 with mixed encoding types."
  (let* ((ecard-21 "BEGIN:VCARD
VERSION:2.1
FN:Test Mixed
NOTE;ENCODING=QUOTED-PRINTABLE:Hello=20World
PHOTO;ENCODING=BASE64;TYPE=JPEG:SGVsbG8=
END:VCARD")
         (vc (ecard-compat-parse-21 ecard-21)))
    (should (ecard-p vc))
    (should (string= (ecard-get-property-value vc 'note) "Hello World"))
    ;; Photo should be a data URI
    (let ((photo (ecard-get-property-value vc 'photo)))
      (should (string-prefix-p "data:" photo)))))

;;; Version detection edge cases

(ert-deftest ecard-compat-detect-version-missing ()
  "Test detection when VERSION is missing entirely."
  (should (null (ecard-compat--detect-version "BEGIN:VCARD\nFN:Test\nEND:VCARD"))))

(ert-deftest ecard-compat-detect-version-with-spaces ()
  "Test detection with spaces around version number."
  (should (eq (ecard-compat--detect-version "VERSION: 3.0") 'v30)))

(provide 'ecard-compat-test)
;;; ecard-compat-test.el ends here
