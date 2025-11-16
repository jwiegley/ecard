;;; vcard.el --- Complete vCard 4.0 (RFC 6350) parser and serializer -*- lexical-binding: t; -*-

;; Copyright (C) 2025 John Wiegley

;; Author: John Wiegley <johnw@gnu.org>
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: comm, data, vcard
;; URL: https://github.com/jwiegley/dot-emacs

;;; Commentary:

;; This package provides complete vCard 4.0 (RFC 6350) support for Emacs
;; using EIEIO object-oriented programming.
;;
;; Features:
;; - Full RFC 6350 section 6 property support
;; - EIEIO-based object model
;; - Robust parsing with line unfolding and value unescaping
;; - Proper serialization with line folding and value escaping
;; - UTF-8 support
;; - Validation of required properties
;;
;; Example usage:
;;
;;   ;; Parse from string
;;   (setq card (vcard-parse "BEGIN:VCARD\nVERSION:4.0\nFN:John Doe\nEND:VCARD"))
;;
;;   ;; Parse from file
;;   (setq card (vcard-parse-file "~/contact.vcf"))
;;
;;   ;; Create programmatically
;;   (setq card (vcard-create :fn "John Doe"
;;                            :email "john@example.com"
;;                            :tel "+1-555-1234"))
;;
;;   ;; Serialize to string
;;   (vcard-serialize card)
;;
;;   ;; Write to file
;;   (vcard-write-file card "~/output.vcf")

;;; Code:

(require 'cl-lib)
(require 'eieio)

;;; Custom group

(defgroup vcard nil
  "Library for vCard 4.0 (RFC 6350) support."
  :group 'comm
  :prefix "vcard-")

;;; Error conditions

(define-error 'vcard-parse-error "vCard parse error")
(define-error 'vcard-validation-error "vCard validation error")

;;; EIEIO Classes

(defclass vcard-property ()
  ((group
    :initarg :group
    :initform nil
    :type (or null string)
    :documentation "Property group prefix (e.g., \"item1\" in \"item1.TEL\").")
   (name
    :initarg :name
    :initform ""
    :type string
    :documentation "Property name in uppercase (e.g., \"TEL\", \"EMAIL\").")
   (parameters
    :initarg :parameters
    :initform nil
    :type list
    :documentation "Property parameters as alist ((PARAM-NAME . param-value) ...).")
   (value
    :initarg :value
    :initform ""
    :type (or string list)
    :documentation "Property value; list for structured properties (N, ADR)."))
  "Represents a single vCard property with optional group, parameters, and value.")

(defclass vcard ()
  ((version
    :initarg :version
    :initform nil
    :type list
    :documentation "VERSION property (always \"4.0\").")
   (source
    :initarg :source
    :initform nil
    :type list
    :documentation "SOURCE property.")
   (kind
    :initarg :kind
    :initform nil
    :type list
    :documentation "KIND property (individual, group, org, location).")
   (xml
    :initarg :xml
    :initform nil
    :type list
    :documentation "XML property.")
   (fn
    :initarg :fn
    :initform nil
    :type list
    :documentation "FN (formatted name) property - REQUIRED, can be multiple.")
   (n
    :initarg :n
    :initform nil
    :type list
    :documentation "N (structured name) property.")
   (nickname
    :initarg :nickname
    :initform nil
    :type list
    :documentation "NICKNAME property.")
   (photo
    :initarg :photo
    :initform nil
    :type list
    :documentation "PHOTO property.")
   (bday
    :initarg :bday
    :initform nil
    :type list
    :documentation "BDAY (birthday) property.")
   (anniversary
    :initarg :anniversary
    :initform nil
    :type list
    :documentation "ANNIVERSARY property.")
   (gender
    :initarg :gender
    :initform nil
    :type list
    :documentation "GENDER property.")
   (adr
    :initarg :adr
    :initform nil
    :type list
    :documentation "ADR (structured address) property.")
   (tel
    :initarg :tel
    :initform nil
    :type list
    :documentation "TEL (telephone) property.")
   (email
    :initarg :email
    :initform nil
    :type list
    :documentation "EMAIL property.")
   (impp
    :initarg :impp
    :initform nil
    :type list
    :documentation "IMPP (instant messaging and presence protocol) property.")
   (lang
    :initarg :lang
    :initform nil
    :type list
    :documentation "LANG (language) property.")
   (geo
    :initarg :geo
    :initform nil
    :type list
    :documentation "GEO (geographical position) property.")
   (tz
    :initarg :tz
    :initform nil
    :type list
    :documentation "TZ (time zone) property.")
   (title
    :initarg :title
    :initform nil
    :type list
    :documentation "TITLE property.")
   (role
    :initarg :role
    :initform nil
    :type list
    :documentation "ROLE property.")
   (logo
    :initarg :logo
    :initform nil
    :type list
    :documentation "LOGO property.")
   (org
    :initarg :org
    :initform nil
    :type list
    :documentation "ORG (organization) property.")
   (member
    :initarg :member
    :initform nil
    :type list
    :documentation "MEMBER property.")
   (related
    :initarg :related
    :initform nil
    :type list
    :documentation "RELATED property.")
   (categories
    :initarg :categories
    :initform nil
    :type list
    :documentation "CATEGORIES property.")
   (note
    :initarg :note
    :initform nil
    :type list
    :documentation "NOTE property.")
   (prodid
    :initarg :prodid
    :initform nil
    :type list
    :documentation "PRODID (product identifier) property.")
   (rev
    :initarg :rev
    :initform nil
    :type list
    :documentation "REV (revision) property.")
   (sound
    :initarg :sound
    :initform nil
    :type list
    :documentation "SOUND property.")
   (uid
    :initarg :uid
    :initform nil
    :type list
    :documentation "UID (unique identifier) property.")
   (clientpidmap
    :initarg :clientpidmap
    :initform nil
    :type list
    :documentation "CLIENTPIDMAP property.")
   (url
    :initarg :url
    :initform nil
    :type list
    :documentation "URL property.")
   (key
    :initarg :key
    :initform nil
    :type list
    :documentation "KEY (public key) property.")
   (fburl
    :initarg :fburl
    :initform nil
    :type list
    :documentation "FBURL (free/busy URL) property.")
   (caladruri
    :initarg :caladruri
    :initform nil
    :type list
    :documentation "CALADRURI (calendar address URI) property.")
   (caluri
    :initarg :caluri
    :initform nil
    :type list
    :documentation "CALURI (calendar URI) property.")
   (extended
    :initarg :extended
    :initform nil
    :type list
    :documentation "Alist for X-* properties: ((x-name . (list of vcard-property)) ...)."))
  "Represents a complete vCard 4.0 object with all RFC 6350 section 6 properties.")

;;; Internal utility functions

(defun vcard--unfold-lines (text)
  "Unfold vCard TEXT by removing CRLF followed by space or tab.
Returns a list of unfolded lines.
Per RFC 6350, the CRLF is removed but the space/tab is kept.

Performance: Uses list accumulation for O(n) string building
instead of O(n²) concatenation."
  (let ((lines (split-string text "[\r\n]+" t)))
    (cl-loop with result = nil
             with current-parts = nil  ; Accumulate line parts in reverse
             for line in lines
             do (if (string-match-p "^[ \t]" line)
                    ;; Continuation line - accumulate the continuation (without leading space/tab)
                    (push (substring line 1) current-parts)
                  ;; New property line - complete previous line if any
                  (progn
                    (when current-parts
                      ;; Build completed line from accumulated parts - O(n)
                      (push (mapconcat #'identity (nreverse current-parts) "") result)
                      (setq current-parts nil))
                    ;; Start new line
                    (setq current-parts (list line))))
             finally (when current-parts
                       ;; Complete final line
                       (push (mapconcat #'identity (nreverse current-parts) "") result))
             finally return (nreverse result))))

(defun vcard--unescape-value (value)
  "Unescape vCard VALUE according to RFC 6350.
Handles \\n (newline), \\\\ (backslash), \\, (comma), \\; (semicolon)."
  (let ((result "")
        (i 0)
        (len (length value)))
    (while (< i len)
      (let ((char (aref value i)))
        (if (and (= char ?\\) (< (1+ i) len))
            (let ((next (aref value (1+ i))))
              (setq result (concat result
                                   (pcase next
                                     (?n "\n")
                                     (?\\ "\\")
                                     (?, ",")
                                     (?\; ";")
                                     (_ (string next)))))
              (setq i (+ i 2)))
          (setq result (concat result (string char)))
          (setq i (1+ i)))))
    result))

(defun vcard--split-text-list (value-string)
  "Split text-list VALUE-STRING on unescaped commas.
Returns list of unescaped component values.
Used for CATEGORIES and NICKNAME properties."
  (let ((result nil)
        (current "")
        (i 0)
        (len (length value-string)))
    (while (< i len)
      (let ((char (aref value-string i)))
        (cond
         ;; Escaped character
         ((and (= char ?\\) (< (1+ i) len))
          (setq current (concat current (substring value-string i (+ i 2))))
          (setq i (+ i 2)))
         ;; Unescaped comma - split here
         ((= char ?,)
          (push (vcard--unescape-value current) result)
          (setq current "")
          (setq i (1+ i)))
         ;; Regular character
         (t
          (setq current (concat current (string char)))
          (setq i (1+ i))))))
    ;; Add final component
    (when (> (length current) 0)
      (push (vcard--unescape-value current) result))
    (nreverse result)))

(defun vcard--escape-value (value)
  "Escape vCard VALUE according to RFC 6350.
Escapes newlines (\\n), backslashes (\\\\), commas (\\,), semicolons (\\;)."
  (when value
    (setq value (replace-regexp-in-string "\\\\" "\\\\\\\\" value))
    (setq value (replace-regexp-in-string "\n" "\\\\n" value))
    (setq value (replace-regexp-in-string "," "\\\\," value))
    (setq value (replace-regexp-in-string ";" "\\\\;" value))
    value))

(defun vcard--parse-parameters (param-string)
  "Parse vCard parameter string PARAM-STRING into alist.
Returns ((PARAM-NAME . param-value) ...)."
  (when (and param-string (not (string-empty-p param-string)))
    (let ((params (split-string param-string ";" t "[ \t]*"))
          (result nil))
      (dolist (param params)
        (if (string-match "^\\([^=]+\\)=\\(.+\\)$" param)
            (let ((key (upcase (match-string 1 param)))
                  (val (match-string 2 param)))
              ;; Remove quotes if present
              (when (and (>= (length val) 2)
                         (= (aref val 0) ?\")
                         (= (aref val (1- (length val))) ?\"))
                (setq val (substring val 1 -1)))
              (push (cons key val) result))
          ;; Parameter without value
          (push (cons (upcase param) t) result)))
      (nreverse result))))

(defun vcard--format-parameters (parameters)
  "Format PARAMETERS alist into vCard parameter string.
Returns string like \"PARAM1=val1;PARAM2=val2\" or empty string."
  (if (null parameters)
      ""
    (mapconcat (lambda (param)
                 (let ((key (car param))
                       (val (cdr param)))
                   (if (eq val t)
                       key
                     (format "%s=%s" key
                             (if (string-match-p "[;:,]" val)
                                 (format "\"%s\"" val)
                               val)))))
               parameters
               ";")))

(defun vcard--parse-property-line (line)
  "Parse a single vCard property LINE.
Returns a plist (:group GROUP :name NAME :parameters PARAMS :value VALUE)."
  ;; Group prefix pattern: only valid group names (alphanumeric, underscore, hyphen)
  ;; This prevents dots in parameter values from being misinterpreted as group separators
  (unless (string-match "^\\(?:\\([a-zA-Z0-9_-]+\\)\\.\\)?\\([^;:]+\\)\\(?:;\\([^:]*\\)\\)?:\\(.*\\)$" line)
    (signal 'vcard-parse-error (list "Invalid property line" line)))

  (let* ((group (match-string 1 line))
         (name (upcase (match-string 2 line)))
         (param-string (match-string 3 line))
         (value-string (match-string 4 line))
         (parameters (vcard--parse-parameters param-string))
         (value (vcard--unescape-value value-string)))

    ;; Parse structured values for N, ADR, ORG, and GENDER (semicolon-separated components)
    (when (member name '("N" "ADR" "ORG" "GENDER"))
      (setq value (mapcar #'vcard--unescape-value
                          (split-string value-string ";" nil))))

    ;; Parse text-list values for CATEGORIES and NICKNAME (comma-separated components)
    (when (member name '("CATEGORIES" "NICKNAME"))
      (setq value (vcard--split-text-list value-string)))

    (list :group group
          :name name
          :parameters parameters
          :value value)))

(defun vcard--property-slot-name (prop-name)
  "Convert property name PROP-NAME to slot symbol.
E.g., \"TEL\" -> tel, \"CALADRURI\" -> caladruri."
  (intern (downcase prop-name)))

(defun vcard--is-cardinality-one-property-p (prop-name)
  "Return non-nil if PROP-NAME has cardinality *1 (at most one).
Per RFC 6350, these properties can appear at most once:
N, BDAY, ANNIVERSARY, GENDER, REV, PRODID, UID, KIND."
  (member prop-name '("N" "BDAY" "ANNIVERSARY" "GENDER" "REV"
                      "PRODID" "UID" "KIND")))

(defun vcard--add-property-to-vcard (vc prop-plist)
  "Add property PROP-PLIST to vcard object VC.
PROP-PLIST is a plist from `vcard--parse-property-line'.
Enforces cardinality constraints for *1 properties."
  (let* ((name (plist-get prop-plist :name))
         (prop (vcard-property
                :group (plist-get prop-plist :group)
                :name name
                :parameters (plist-get prop-plist :parameters)
                :value (plist-get prop-plist :value))))

    (cond
     ;; Extended properties (X-*)
     ((string-prefix-p "X-" name)
      (let* ((extended (oref vc extended))
             (existing (assoc name extended)))
        (if existing
            (setcdr existing (append (cdr existing) (list prop)))
          (oset vc extended (append extended (list (cons name (list prop))))))))

     ;; Standard properties
     (t
      (let ((slot (vcard--property-slot-name name)))
        (when (slot-exists-p vc slot)
          (let ((current (slot-value vc slot)))
            ;; Enforce cardinality *1 constraint
            (when (and (vcard--is-cardinality-one-property-p name)
                       current)
              (signal 'vcard-validation-error
                      (list (format "Property %s can appear at most once (cardinality *1)" name))))
            (setf (slot-value vc slot) (append current (list prop))))))))))

(defun vcard--fold-line (line)
  "Fold LINE at 75 octets using space continuation.
Returns list of folded lines."
  (let ((max-len 75)
        (lines nil)
        (current-line "")
        (octets (encode-coding-string line 'utf-8)))

    (if (<= (length octets) max-len)
        (list line)
      ;; Need to fold - be conservative and fold at character boundaries
      (let ((chars (string-to-list line))
            (current-octets 0))
        (dolist (char chars)
          (let* ((char-str (string char))
                 (char-octets (length (encode-coding-string char-str 'utf-8))))
            (if (and (> current-octets 0)
                     (> (+ current-octets char-octets) max-len))
                ;; Need to fold here
                (progn
                  (push current-line lines)
                  (setq current-line (concat " " char-str))
                  (setq current-octets (1+ char-octets)))
              ;; Add to current line
              (setq current-line (concat current-line char-str))
              (setq current-octets (+ current-octets char-octets)))))
        (when (not (string-empty-p current-line))
          (push current-line lines))
        (nreverse lines)))))

(defun vcard--format-property (prop)
  "Format vcard property PROP into vCard line (without folding).
Returns formatted string."
  (let* ((group (oref prop group))
         (name (oref prop name))
         (parameters (oref prop parameters))
         (value (oref prop value))
         (param-str (vcard--format-parameters parameters))
         (value-str (if (listp value)
                        ;; Text-list properties (CATEGORIES, NICKNAME) use comma separator
                        (if (member name '("CATEGORIES" "NICKNAME"))
                            (mapconcat #'vcard--escape-value value ",")
                          ;; Structured properties (N, ADR, ORG, GENDER) use semicolon separator
                          (mapconcat #'vcard--escape-value value ";"))
                      (vcard--escape-value value))))

    (concat (if group (concat group ".") "")
            name
            (if (not (string-empty-p param-str)) (concat ";" param-str) "")
            ":"
            value-str)))

(defun vcard--serialize-properties (props)
  "Serialize list of vcard properties PROPS into vCard lines.
Returns list of folded lines."
  (let ((lines nil))
    (dolist (prop props)
      (let ((line (vcard--format-property prop)))
        (setq lines (append lines (vcard--fold-line line)))))
    lines))

;;; Public API

(defun vcard--validate-pref-parameters (vc)
  "Validate PREF parameter values are integers 1-100.
VC is the vcard object to validate.
Signals `vcard-validation-error' if any PREF value is out of range."
  (dolist (slot '(fn n nickname photo bday anniversary gender adr tel email
                  impp lang geo tz title role logo org member related
                  categories note prodid rev sound uid clientpidmap url
                  key fburl caladruri caluri source kind xml))
    (let ((props (slot-value vc slot)))
      (dolist (prop props)
        (let ((params (oref prop parameters)))
          (when params
            (let ((pref-param (assoc "PREF" params)))
              (when pref-param
                (let* ((pref-value (cdr pref-param))
                       ;; Parse as number and check if it's a valid integer in range
                       (pref-num (ignore-errors (string-to-number pref-value)))
                       (pref-int (and pref-num (truncate pref-num))))
                  (unless (and pref-int
                               (= pref-num pref-int)  ; Ensure it's actually an integer, not a float
                               (>= pref-int 1)
                               (<= pref-int 100))
                    (signal 'vcard-validation-error
                            (list (format "PREF parameter must be integer 1-100, got: %s"
                                          pref-value)))))))))))))

(defun vcard--validate-vcard (vc)
  "Validate required properties for vcard object VC.
Signals `vcard-validation-error' if validation fails."
  ;; Validate required properties
  (unless (oref vc version)
    (signal 'vcard-validation-error '("Missing VERSION property")))

  (unless (oref vc fn)
    (signal 'vcard-validation-error '("Missing FN (formatted name) property")))

  ;; Validate VERSION is 4.0
  (let ((version-prop (car (oref vc version))))
    (unless (and version-prop
                 (string= (oref version-prop value) "4.0"))
      (signal 'vcard-validation-error
              (list "Unsupported VERSION"
                    (if version-prop (oref version-prop value) "nil")))))

  ;; Validate KIND value if present
  (let ((kind-props (oref vc kind)))
    (when kind-props
      (let* ((kind-prop (car kind-props))
             (kind-value (downcase (oref kind-prop value))))
        (unless (member kind-value '("individual" "group" "org" "location"))
          (signal 'vcard-validation-error
                  (list (format "Invalid KIND value: %s (must be individual, group, org, or location)"
                                (oref kind-prop value))))))))

  ;; Validate MEMBER/KIND relationship
  (let ((member-props (oref vc member))
        (kind-props (oref vc kind)))
    (when (and member-props
               (or (null kind-props)
                   (not (string= (downcase (oref (car kind-props) value)) "group"))))
      (signal 'vcard-validation-error
              '("MEMBER property requires KIND to be 'group'"))))

  ;; Validate PREF parameter ranges across all properties
  (vcard--validate-pref-parameters vc))

;;;###autoload
(defun vcard-parse-multiple (text)
  "Parse vCard TEXT into a list of vcard objects.
TEXT can contain one or more vCard 4.0 records.
Returns a list of vcard objects, even if only one vCard is present.
Signals `vcard-parse-error' if parsing fails.
Signals `vcard-validation-error' if required properties are missing."
  (let* ((lines (vcard--unfold-lines text))
         (vcards nil)
         (current-vc nil)
         (in-vcard nil))

    (dolist (line lines)
      (cond
       ((string-match-p "^BEGIN:VCARD" line)
        (when in-vcard
          (signal 'vcard-parse-error '("Nested BEGIN:VCARD not allowed")))
        (setq in-vcard t)
        (setq current-vc (vcard)))

       ((string-match-p "^END:VCARD" line)
        (unless in-vcard
          (signal 'vcard-parse-error '("END:VCARD without BEGIN:VCARD")))
        (setq in-vcard nil)
        ;; Validate and add completed vCard
        (vcard--validate-vcard current-vc)
        (push current-vc vcards)
        (setq current-vc nil))

       (in-vcard
        (let ((prop-plist (vcard--parse-property-line line)))
          (vcard--add-property-to-vcard current-vc prop-plist)))))

    (when in-vcard
      (signal 'vcard-parse-error '("Missing END:VCARD")))

    (nreverse vcards)))

;;;###autoload
(defun vcard-parse (text)
  "Parse vCard TEXT into a vcard object or list of vcard objects.
TEXT should contain one or more complete vCard 4.0 records.

If TEXT contains a single vCard, returns a single vcard object.
If TEXT contains multiple vCards, returns a list of vcard objects.
If TEXT contains no vCards, signals an error.

Signals `vcard-parse-error' if parsing fails.
Signals `vcard-validation-error' if required properties are missing.

For explicit control over return type, use `vcard-parse-multiple'
which always returns a list."
  (let ((vcards (vcard-parse-multiple text)))
    (cond
     ((= (length vcards) 0)
      (signal 'vcard-parse-error '("No vCards found in input")))
     ((= (length vcards) 1)
      (car vcards))
     (t
      vcards))))

;;;###autoload
(defun vcard-parse-file-multiple (filename)
  "Parse vCard from FILENAME and return list of vcard objects.
Always returns a list, even if only one vCard is present.
Signals `vcard-parse-error' if parsing fails."
  (with-temp-buffer
    (insert-file-contents filename)
    (vcard-parse-multiple (buffer-string))))

;;;###autoload
(defun vcard-parse-file (filename)
  "Parse vCard from FILENAME and return vcard object or list.
If FILENAME contains a single vCard, returns a single vcard object.
If FILENAME contains multiple vCards, returns a list of vcard objects.
Signals `vcard-parse-error' if parsing fails.

For explicit control over return type, use `vcard-parse-file-multiple'
which always returns a list."
  (with-temp-buffer
    (insert-file-contents filename)
    (vcard-parse (buffer-string))))

;;;###autoload
(defun vcard-parse-buffer-multiple ()
  "Parse vCard from current buffer and return list of vcard objects.
Always returns a list, even if only one vCard is present.
Signals `vcard-parse-error' if parsing fails."
  (vcard-parse-multiple (buffer-string)))

;;;###autoload
(defun vcard-parse-buffer ()
  "Parse vCard from current buffer and return vcard object or list.
If buffer contains a single vCard, returns a single vcard object.
If buffer contains multiple vCards, returns a list of vcard objects.
Signals `vcard-parse-error' if parsing fails.

For explicit control over return type, use `vcard-parse-buffer-multiple'
which always returns a list."
  (vcard-parse (buffer-string)))

;;;###autoload
(defun vcard-serialize (vc)
  "Serialize vcard object VC to vCard 4.0 text string.
Returns a properly formatted and folded vCard string."
  (let ((lines '("BEGIN:VCARD" "VERSION:4.0")))

    ;; Serialize all properties in defined order
    (dolist (slot '(source kind xml fn n nickname photo bday anniversary gender
                    adr tel email impp lang geo tz title role logo org member related
                    categories note prodid rev sound uid clientpidmap url
                    key fburl caladruri caluri))
      (let ((props (slot-value vc slot)))
        (when props
          (setq lines (append lines (vcard--serialize-properties props))))))

    ;; Serialize extended properties
    (let ((extended (oref vc extended)))
      (dolist (entry extended)
        (let ((props (cdr entry)))
          (setq lines (append lines (vcard--serialize-properties props))))))

    ;; Add END:VCARD
    (setq lines (append lines '("END:VCARD")))

    ;; Join with CRLF as per RFC 6350
    (mapconcat #'identity lines "\r\n")))

;;;###autoload
(defun vcard-serialize-multiple (vcards)
  "Serialize list of vcard objects VCARDS to vCard 4.0 text string.
Each vCard is separated by a newline.
Returns a properly formatted and folded vCard string."
  (mapconcat #'vcard-serialize vcards "\r\n"))

;;;###autoload
(defun vcard-write-file (vc filename)
  "Write vcard object VC to FILENAME.
Creates or overwrites FILENAME with serialized vCard."
  (with-temp-buffer
    (insert (vcard-serialize vc))
    (write-region (point-min) (point-max) filename)))

;;;###autoload
(defun vcard-create (&rest args)
  "Create a new vcard object with properties from ARGS.
ARGS is a plist of property keywords and values.

Supported keywords:
  :fn STRING - Formatted name (required)
  :n LIST - Structured name (family given additional prefix suffix)
  :email STRING-OR-LIST - Email address(es)
  :tel STRING-OR-LIST - Telephone number(s)
  :adr LIST-OR-LISTS - Address(es)
  :org STRING - Organization
  :title STRING - Job title
  :role STRING - Role
  :url STRING - URL
  :note STRING - Note
  :uid STRING - Unique identifier
  :bday STRING - Birthday
  :anniversary STRING - Anniversary
  :gender STRING - Gender
  :nickname STRING - Nickname
  :categories STRING-OR-LIST - Categories
  :geo STRING - Geographical position
  :tz STRING - Time zone
  :photo STRING - Photo data
  :logo STRING - Logo data
  :sound STRING - Sound data
  :key STRING - Public key
  :rev STRING - Revision timestamp
  :prodid STRING - Product ID
  :kind STRING - Kind (individual, group, org, location)
  :source STRING - Source

Example:
  (vcard-create :fn \"John Doe\"
                :email \"john@example.com\"
                :tel \"+1-555-1234\"
                :org \"Example Corp\")"

  (let ((vc (vcard))
        (fn (plist-get args :fn))
        (n (plist-get args :n))
        (email (plist-get args :email))
        (tel (plist-get args :tel))
        (adr (plist-get args :adr))
        (org (plist-get args :org))
        (title (plist-get args :title))
        (role (plist-get args :role))
        (url (plist-get args :url))
        (note (plist-get args :note))
        (uid (plist-get args :uid))
        (bday (plist-get args :bday))
        (anniversary (plist-get args :anniversary))
        (gender (plist-get args :gender))
        (nickname (plist-get args :nickname))
        (categories (plist-get args :categories))
        (geo (plist-get args :geo))
        (tz (plist-get args :tz))
        (photo (plist-get args :photo))
        (logo (plist-get args :logo))
        (sound (plist-get args :sound))
        (key (plist-get args :key))
        (rev (plist-get args :rev))
        (prodid (plist-get args :prodid))
        (kind (plist-get args :kind))
        (source (plist-get args :source)))

    ;; Set VERSION:4.0
    (oset vc version (list (vcard-property :name "VERSION" :value "4.0")))

    ;; Helper macro to add simple properties
    (cl-macrolet ((add-prop (slot-name value)
                    `(when ,value
                       (oset vc ,slot-name
                             (if (listp ,value)
                                 (mapcar (lambda (v)
                                           (vcard-property
                                            :name (upcase (symbol-name ',slot-name))
                                            :value v))
                                         ,value)
                               (list (vcard-property
                                      :name (upcase (symbol-name ',slot-name))
                                      :value ,value)))))))

      ;; Add all properties
      (add-prop fn fn)
      (add-prop email email)
      (add-prop tel tel)
      (add-prop title title)
      (add-prop role role)
      (add-prop url url)
      (add-prop note note)
      (add-prop uid uid)
      (add-prop bday bday)
      (add-prop anniversary anniversary)
      (add-prop gender gender)
      (add-prop nickname nickname)
      (add-prop categories categories)
      (add-prop geo geo)
      (add-prop tz tz)
      (add-prop photo photo)
      (add-prop logo logo)
      (add-prop sound sound)
      (add-prop key key)
      (add-prop rev rev)
      (add-prop prodid prodid)
      (add-prop kind kind)
      (add-prop source source))

    ;; Handle N (structured name) - can be a list
    (when n
      (oset vc n (list (vcard-property :name "N" :value n))))

    ;; Handle ORG (structured organization) - convert string to list
    (when org
      (oset vc org (list (vcard-property :name "ORG"
                                         :value (if (listp org) org (list org))))))

    ;; Handle GENDER (structured) - convert string to list
    (when gender
      (oset vc gender (list (vcard-property :name "GENDER"
                                            :value (if (listp gender) gender (list gender))))))

    ;; Handle ADR (structured address) - can be list of lists
    (when adr
      (oset vc adr
            (if (and (listp adr) (listp (car adr)) (stringp (car (car adr))))
                ;; List of addresses
                (mapcar (lambda (a) (vcard-property :name "ADR" :value a)) adr)
              ;; Single address
              (list (vcard-property :name "ADR" :value adr)))))

    ;; Validate FN is present
    (unless (oref vc fn)
      (signal 'vcard-validation-error '("FN (formatted name) is required")))

    vc))

;;; Helper functions for property access

(defun vcard-get-property-values (vc property-name)
  "Get all values for PROPERTY-NAME from vcard object VC.
Returns list of values (strings or lists for structured properties).
PROPERTY-NAME should be a lowercase symbol (e.g., \\='fn, \\='email)."
  (let ((props (slot-value vc property-name)))
    (mapcar (lambda (prop) (oref prop value)) props)))

(defun vcard-get-property-value (vc property-name)
  "Get first value for PROPERTY-NAME from vcard object VC.
Returns string or list for structured properties, or nil if not found.
PROPERTY-NAME should be a lowercase symbol (e.g., \\='fn, \\='email)."
  (let ((props (slot-value vc property-name)))
    (when props
      (oref (car props) value))))

(defun vcard-set-property (vc property-name value &optional parameters group)
  "Set PROPERTY-NAME in vcard object VC to VALUE.
Replaces all existing values for this property.
PROPERTY-NAME should be a lowercase symbol (e.g., \\='fn, \\='email).
VALUE is a string or list (for structured properties).
Optional PARAMETERS is an alist of property parameters.
Optional GROUP is the property group string."
  (let ((prop (vcard-property
               :name (upcase (symbol-name property-name))
               :value value
               :parameters parameters
               :group group)))
    (setf (slot-value vc property-name) (list prop))))

(defun vcard-add-property (vc property-name value &optional parameters group)
  "Add PROPERTY-NAME to vcard object VC with VALUE.
Appends to existing values for this property.
PROPERTY-NAME should be a lowercase symbol (e.g., \\='fn, \\='email).
VALUE is a string or list (for structured properties).
Optional PARAMETERS is an alist of property parameters.
Optional GROUP is the property group string."
  (let ((prop (vcard-property
               :name (upcase (symbol-name property-name))
               :value value
               :parameters parameters
               :group group))
        (existing (slot-value vc property-name)))
    (setf (slot-value vc property-name) (append existing (list prop)))))

(provide 'vcard)
;;; vcard.el ends here
