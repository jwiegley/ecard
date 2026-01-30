;;; ecard-sync.el --- Sync org-contacts with CardDAV -*- lexical-binding: t; -*-

;; Copyright (C) 2025 John Wiegley

;; Author: John Wiegley <johnw@gnu.org>
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1") (ecard "1.0.0") (ecard-carddav "1.0.0"))
;; Keywords: comm, data, carddav, contacts
;; URL: https://github.com/jwiegley/dot-emacs

;;; Commentary:

;; Synchronize org-contacts entries with a CardDAV server using
;; the existing ecard-carddav infrastructure.
;;
;; Setup:
;;
;;   (require 'ecard-sync)
;;
;;   ;; Configure servers via ecard-carddav-servers (see ecard-carddav.el)
;;   ;; Then set which server/addressbook to sync with:
;;   (setq ecard-sync-server (car ecard-carddav-servers)
;;         ecard-sync-addressbook-name "contacts")
;;
;;   ;; In your org-contacts file, position on a contact and run:
;;   M-x ecard-sync-entry
;;
;; Org-contacts properties mapped:
;; - FN: headline text (formatted name)
;; - N: split from headline (structured name)
;; - EMAIL/EMAIL2: :EMAIL and :EMAIL2 properties
;; - TEL: :PHONE property
;; - ADR: :ADDRESS property (structured)
;; - ORG: :ORG property
;; - NOTE: entry body text (below property drawer)
;; - BDAY: :BIRTHDAY property
;; - GEO: :LOCATION property (lat,lon)
;; - CATEGORIES: org tags
;; - UID: org :ID property (unified identifier)
;;
;; ID Synchronization:
;; - The org :ID property is used directly as the vCard UID
;; - If a vCard already exists on the server (found by FN search),
;;   the org entry's :ID is CHANGED to match the server's UID
;; - If creating a new vCard, the existing org :ID is used as the UID

;;; Code:

(require 'ecard)
(require 'ecard-carddav)
(require 'org)
(require 'org-element)
(require 'subr-x)

(defvar url-http-data)

;;; Encoding fix for url.el

;; In interactive Emacs, url.el header strings may be multibyte (even
;; if ASCII-only).  url-http-create-request concatenates all headers
;; and body into one string via `concat'.  When a multibyte header is
;; concatenated with a unibyte UTF-8 body, bytes >= 0x80 in the body
;; become two-byte "eight-bit" characters, causing string-bytes >
;; length.  This triggers the Bug#23750 check:
;;   (unless (= (string-bytes request) (length request))
;;     (error "Multibyte text in HTTP request"))
;;
;; The fix: when body data is present, build headers without the body
;; (which passes the check), then reconstruct the full request as a
;; unibyte string.

(defun ecard-sync--fix-multibyte-request (orig-fn)
  "Around advice for `url-http-create-request' to prevent multibyte errors.
Builds headers separately, then appends body as a unibyte string.

The problem: `url-http-create-request' concatenates headers and body
via `concat'.  Headers are multibyte (even if ASCII-only) in
interactive Emacs.  When a unibyte UTF-8 body containing bytes
>= 0x80 is concatenated with multibyte headers, those bytes become
two-byte eight-bit characters, corrupting the data.

The fix: build headers without the body (which passes url.el's
check), encode headers to unibyte (they are ASCII), then concat
two unibyte strings so no byte promotion occurs."
  (let ((saved-data url-http-data))
    (if (and saved-data
             (stringp saved-data)
             (not (zerop (length saved-data))))
        ;; Body data present — handle potential multibyte concat issue
        (progn
          (setq url-http-data nil)
          (unwind-protect
              (let* ((headers (funcall orig-fn))
                     ;; Ensure body is unibyte UTF-8
                     (body (if (multibyte-string-p saved-data)
                               (encode-coding-string saved-data 'utf-8)
                             saved-data))
                     (byte-count (string-bytes body))
                     ;; Build the header portion (all ASCII) and convert
                     ;; to unibyte so the concat with body stays unibyte.
                     (header-bytes
                      (encode-coding-string
                       (concat (substring headers 0 (- (length headers) 2))
                               (format "Content-length: %d\r\n" byte-count)
                               "\r\n")
                       'utf-8)))
                ;; Both header-bytes and body are unibyte — concat is safe.
                (concat header-bytes body))
            (setq url-http-data saved-data)))
      ;; No body — just call original
      (funcall orig-fn))))

(advice-add 'url-http-create-request :around #'ecard-sync--fix-multibyte-request)

;;; Custom group

(defgroup ecard-sync nil
  "Sync org-contacts with CardDAV using ecard-carddav."
  :group 'ecard
  :prefix "ecard-sync-")

;;; Customization variables

(defcustom ecard-sync-server nil
  "CardDAV server to sync with.
Should be an element from `ecard-carddav-servers' or an
`ecard-carddav-server' object."
  :type '(choice (const :tag "Not configured" nil)
                 (sexp :tag "Server configuration"))
  :group 'ecard-sync)

(defcustom ecard-sync-addressbook-name nil
  "Name of addressbook to sync with on the server.
If nil, uses the first addressbook found."
  :type '(choice (const :tag "First available" nil)
                 (string :tag "Addressbook name"))
  :group 'ecard-sync)

(defcustom ecard-sync-prefer-local t
  "When non-nil, prefer local (org) values during merge.
When nil, prefer server values."
  :type 'boolean
  :group 'ecard-sync)

;;; Internal variables

(defvar ecard-sync--debug nil
  "When non-nil, print debug messages during sync.")

(defvar ecard-sync--addressbook-cache nil
  "Cached addressbook object to avoid repeated discovery.")

(defvar ecard-sync--resource-cache nil
  "Cached list of (resource . ecard) for the current addressbook.
Populated on first use by a single multiget request.")

;;; Utility functions

(defun ecard-sync--debug (format-string &rest args)
  "Print debug message if `ecard-sync--debug' is non-nil.
FORMAT-STRING and ARGS are passed to `message'."
  (when ecard-sync--debug
    (apply #'message (concat "[ecard-sync] " format-string) args)))

(defun ecard-sync--get-server ()
  "Get the configured server object.
Normalizes plist configs to server objects."
  (unless ecard-sync-server
    (user-error "ecard-sync-server not configured"))
  (let ((server ecard-sync-server))
    ;; Normalize plist to server object if needed
    (if (ecard-carddav-server-p server)
        server
      ;; Convert plist to server object
      (ecard-carddav-server-create
       :url (plist-get server :url)
       :auth (ecard-carddav-auth-basic-create
              :username (plist-get server :username)
              :password (plist-get server :password))))))

(defun ecard-sync--get-addressbook ()
  "Get the addressbook to sync with.
Discovers addressbooks if needed and caches the result."
  (unless ecard-sync--addressbook-cache
    (let* ((server (ecard-sync--get-server))
           (addressbooks (ecard-carddav-discover-addressbooks server)))
      (unless addressbooks
        (user-error "No addressbooks found on server"))
      (setq ecard-sync--addressbook-cache
            (if ecard-sync-addressbook-name
                (or (cl-find ecard-sync-addressbook-name addressbooks
                             :key (lambda (ab) (oref ab display-name))
                             :test #'string=)
                    (user-error "Addressbook '%s' not found" ecard-sync-addressbook-name))
              (car addressbooks)))))
  ecard-sync--addressbook-cache)

(defun ecard-sync-clear-cache ()
  "Clear the addressbook and resource caches.
Call this if server configuration changes."
  (interactive)
  (setq ecard-sync--addressbook-cache nil
        ecard-sync--resource-cache nil)
  (message "ecard-sync cache cleared"))

(defun ecard-sync--split-name (full-name)
  "Split FULL-NAME into structured name components.
Returns list (family given additional prefix suffix).
Handles \"Firstname Lastname\" and \"Firstname Middlename Lastname\"."
  (let ((parts (split-string (string-trim full-name) "\\s-+" t)))
    (cond
     ;; Single name
     ((= (length parts) 1)
      (list "" (car parts) "" "" ""))
     ;; Two names: Given Family
     ((= (length parts) 2)
      (list (cadr parts) (car parts) "" "" ""))
     ;; Three or more names: Given Additional Family
     (t
      (list (car (last parts))
            (car parts)
            (mapconcat #'identity (butlast (cdr parts)) " ")
            ""
            "")))))

(defun ecard-sync--clean-string (str)
  "Strip invisible Unicode control characters from STR.
Removes bidirectional controls (U+200E-200F, U+202A-202E,
U+2066-2069), zero-width characters (U+200B-200D, U+FEFF),
and other non-printing formatting characters that commonly
appear from copy-paste."
  (when str
    (replace-regexp-in-string
     "[\x200b-\x200f\x202a-\x202e\x2060-\x2069\xfeff\xad]"
     "" str)))

(defun ecard-sync--parse-location (location-str)
  "Parse LOCATION-STR (lat,lon) into geo URI format.
Returns string like \"geo:37.386,-122.082\" or nil."
  (when (and location-str (string-match "^\\([0-9.-]+\\),\\([0-9.-]+\\)$" location-str))
    (format "geo:%s,%s" (match-string 1 location-str) (match-string 2 location-str))))

(defun ecard-sync--format-location (geo-uri)
  "Format GEO-URI into lat,lon string.
Returns string like \"37.386,-122.082\" or nil."
  (when (and geo-uri (string-match "^geo:\\([0-9.-]+\\),\\([0-9.-]+\\)" geo-uri))
    (format "%s,%s" (match-string 1 geo-uri) (match-string 2 geo-uri))))

(defun ecard-sync--parse-rev-timestamp (rev-string)
  "Parse vCard REV timestamp to Emacs time.
REV-STRING should be in ISO 8601 basic format: YYYYMMDDTHHMMSSZ.
Returns Emacs time value or nil if parsing fails."
  (when (and rev-string
             (string-match
              "^\\([0-9]\\{4\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)T\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)Z$"
              rev-string))
    (encode-time
     (string-to-number (match-string 6 rev-string))
     (string-to-number (match-string 5 rev-string))
     (string-to-number (match-string 4 rev-string))
     (string-to-number (match-string 3 rev-string))
     (string-to-number (match-string 2 rev-string))
     (string-to-number (match-string 1 rev-string))
     t)))

(defun ecard-sync--parse-org-timestamp (timestamp-string)
  "Parse Org-mode timestamp to Emacs time.
TIMESTAMP-STRING should be like \"[2025-01-28 Tue 12:00]\".
Returns Emacs time value or nil if parsing fails."
  (when (and timestamp-string (not (string-empty-p timestamp-string)))
    (condition-case nil
        (let ((decoded (org-parse-time-string timestamp-string)))
          (apply #'encode-time decoded))
      (error nil))))

(defun ecard-sync--empty-value-p (value)
  "Return non-nil if VALUE is semantically empty.
VALUE is considered empty if it is:
- nil
- empty string
- whitespace-only string
- a list where ALL elements are nil or empty/blank strings."
  (cond
   ((null value) t)
   ((stringp value) (string-blank-p value))
   ((listp value)
    (cl-every (lambda (elem)
                (or (null elem)
                    (and (stringp elem) (string-blank-p elem))))
              value))
   (t nil)))

(defun ecard-sync--slot-empty-p (prop-list)
  "Return non-nil if PROP-LIST is semantically empty.
PROP-LIST should be a list of ecard-property objects.
Returns t if the list is nil or if all property values are empty."
  (or (null prop-list)
      (cl-every (lambda (prop)
                  (ecard-sync--empty-value-p (ecard-property-value prop)))
                prop-list)))

;;; Org-to-ecard conversion

;;;###autoload
(defun ecard-sync-org-to-ecard ()
  "Convert current org headline to ecard object.
Returns a new ecard object with properties extracted from org entry."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in org-mode buffer"))

  (save-excursion
    (org-back-to-heading t)
    (let* ((element (org-element-at-point))
           (headline (org-element-property :raw-value element))
           (tags (org-element-property :tags element))
           (fn (string-trim headline))
           (n (ecard-sync--split-name fn))
           (props (org-entry-properties))
           (email (ecard-sync--clean-string (cdr (assoc "EMAIL" props))))
           (email2 (ecard-sync--clean-string (cdr (assoc "EMAIL2" props))))
           (phone (ecard-sync--clean-string (cdr (assoc "PHONE" props))))
           (address (ecard-sync--clean-string (cdr (assoc "ADDRESS" props))))
           (org-name (ecard-sync--clean-string (cdr (assoc "ORG" props))))
           (note (ecard-sync--clean-string
                  (save-excursion
                    (let ((body-end (org-entry-end-position)))
                      (org-end-of-meta-data t)
                      (let ((text (string-trim
                                   (buffer-substring-no-properties
                                    (point) body-end))))
                        (unless (string-empty-p text) text))))))
           (birthday (ecard-sync--clean-string (cdr (assoc "BIRTHDAY" props))))
           (location (ecard-sync--clean-string (cdr (assoc "LOCATION" props))))
           (org-id (cdr (assoc "ID" props)))
           (vc (ecard-create :fn fn :n n)))

      ;; Add email(s)
      (when (and email (not (string-empty-p email)))
        (ecard-set-property vc 'email email))
      (when (and email2 (not (string-empty-p email2)))
        (ecard-add-property vc 'email email2))

      ;; Add phone
      (when (and phone (not (string-empty-p phone)))
        (ecard-set-property vc 'tel phone))

      ;; Add address (parse as structured if contains semicolons)
      (when (and address (not (string-empty-p address)))
        (let ((adr-components (if (string-match-p ";" address)
                                  (split-string address ";")
                                (list "" "" address "" "" "" ""))))
          (ecard-set-property vc 'adr adr-components)))

      ;; Add organization
      (when (and org-name (not (string-empty-p org-name)))
        (ecard-set-property vc 'org (list org-name)))

      ;; Add note
      (when (and note (not (string-empty-p note)))
        (ecard-set-property vc 'note note))

      ;; Add birthday
      (when (and birthday (not (string-empty-p birthday)))
        (ecard-set-property vc 'bday birthday))

      ;; Add location as GEO
      (when (and location (not (string-empty-p location)))
        (let ((geo-uri (ecard-sync--parse-location location)))
          (when geo-uri
            (ecard-set-property vc 'geo geo-uri))))

      ;; Add tags as categories
      (when tags
        (ecard-set-property vc 'categories tags))

      ;; Use org ID as vCard UID
      (when (and org-id (not (string-empty-p org-id)))
        (ecard-set-property vc 'uid org-id))

      ;; Set REV to current timestamp
      (ecard-set-property vc 'rev (format-time-string "%Y%m%dT%H%M%SZ" nil t))

      vc)))

;;; Ecard-to-org update

;;;###autoload
(defun ecard-sync-ecard-to-org (vc &optional update-id)
  "Update current org headline from ecard object VC.
If UPDATE-ID is non-nil, also update the org :ID property from the vCard UID."
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in org-mode buffer"))
  (unless (ecard-p vc)
    (user-error "Argument must be an ecard object"))

  (save-excursion
    (org-back-to-heading t)

    (let ((uid (ecard-get-property-value vc 'uid))
          (emails (ecard-get-property-values vc 'email))
          (tel (ecard-get-property-value vc 'tel))
          (adr (ecard-get-property-value vc 'adr))
          (org-name (ecard-get-property-value vc 'org))
          (note (ecard-get-property-value vc 'note))
          (bday (ecard-get-property-value vc 'bday))
          (geo (ecard-get-property-value vc 'geo))
          (categories (ecard-get-property-value vc 'categories)))

      ;; Update org ID if requested (when adopting server's UID)
      (when (and update-id uid)
        (org-set-property "ID" uid))

      ;; Set EMAIL and EMAIL2
      (when emails
        (let ((email1 (car emails))
              (email2 (cadr emails)))
          (when (and email1 (not (string-blank-p email1)))
            (org-set-property "EMAIL" email1))
          (when (and email2 (not (string-blank-p email2)))
            (org-set-property "EMAIL2" email2))))

      ;; Set PHONE
      (when (and tel (not (string-blank-p tel)))
        (org-set-property "PHONE" tel))

      ;; Set ADDRESS
      (when adr
        (let ((adr-str (if (listp adr)
                           (mapconcat #'identity adr ";")
                         adr)))
          (when (not (string-blank-p adr-str))
            (org-set-property "ADDRESS" adr-str))))

      ;; Set ORG
      (when org-name
        (let ((org-str (if (listp org-name)
                           (car org-name)
                         org-name)))
          (when (and org-str (not (string-blank-p org-str)))
            (org-set-property "ORG" org-str))))

      ;; Set body text from NOTE
      (when (and note (not (string-blank-p note)))
        ;; Remove legacy :NOTE: property if present
        (org-back-to-heading t)
        (when (org-entry-get (point) "NOTE")
          (org-delete-property "NOTE"))
        ;; Replace body text
        (org-back-to-heading t)
        (let ((body-end (org-entry-end-position)))
          (org-end-of-meta-data t)
          (delete-region (point) body-end)
          (insert note "\n")))

      ;; Set BIRTHDAY
      (when (and bday (not (string-blank-p bday)))
        (org-set-property "BIRTHDAY" bday))

      ;; Set LOCATION from GEO
      (when geo
        (let ((location (ecard-sync--format-location geo)))
          (when (and location (not (string-blank-p location)))
            (org-set-property "LOCATION" location))))

      ;; Set tags from CATEGORIES
      (when (and categories (listp categories))
        (org-set-tags categories)))))

;;; Server operations

(defun ecard-sync--ensure-resources (addressbook)
  "Ensure all resources are fetched and cached for ADDRESSBOOK.
Uses a single multiget request to fetch all vCards at once."
  (unless ecard-sync--resource-cache
    (ecard-sync--debug "Fetching all resources via multiget...")
    (let ((resources (ecard-carddav-list-resources addressbook)))
      (if (null resources)
          (setq ecard-sync--resource-cache 'empty)
        (let* ((paths (mapcar (lambda (r) (oref r path)) resources))
               (fetched (ecard-carddav-multiget-resources addressbook paths)))
          (setq ecard-sync--resource-cache
                (mapcar (lambda (r) (cons r (oref r ecard))) fetched))))))
  (if (eq ecard-sync--resource-cache 'empty)
      nil
    ecard-sync--resource-cache))

(defun ecard-sync--find-resource-by-uid (addressbook uid)
  "Find resource in ADDRESSBOOK with matching UID.
Returns resource object or nil."
  (let ((uid-path (concat uid ".vcf"))
        (entries (ecard-sync--ensure-resources addressbook)))
    (cl-find-if (lambda (entry)
                  (let ((r (car entry)))
                    (string-suffix-p uid-path (oref r path))))
                entries)))

(defun ecard-sync--find-resource-by-fn (addressbook fn)
  "Find resource in ADDRESSBOOK with matching formatted name FN.
Returns cons (resource . ecard) or nil.
If multiple matches exist, prompts the user to choose."
  (ecard-sync--debug "Searching for FN=%s" fn)
  (let* ((entries (ecard-sync--ensure-resources addressbook))
         (matches (cl-remove-if-not
                   (lambda (entry)
                     (let* ((vc (cdr entry))
                            (server-fn (ecard-get-property-value vc 'fn)))
                       (and server-fn
                            (string= (downcase server-fn) (downcase fn)))))
                   entries)))
    (cond
     ((null matches) nil)
     ((= (length matches) 1) (car matches))
     (t
      ;; Multiple matches: let user pick
      (let* ((choices
              (mapcar (lambda (entry)
                        (let* ((r (car entry))
                               (vc (cdr entry))
                               (uid (or (ecard-get-property-value vc 'uid) "?"))
                               (email (or (ecard-get-property-value vc 'email) ""))
                               (path (oref r path)))
                          (cons (format "%s  UID:%s  %s" path uid email)
                                entry)))
                      matches))
             (choice (completing-read
                      (format "Multiple vCards for \"%s\" — choose: " fn)
                      (mapcar #'car choices) nil t)))
        (cdr (assoc choice choices)))))))

;;; Merge logic

(defun ecard-sync--determine-prefer-local (synced-string rev-string)
  "Determine effective merge direction from timestamps.
SYNCED-STRING is the Org SYNCED property value (or nil).
REV-STRING is the server vCard REV value (or nil).

Returns the effective value for `ecard-sync-prefer-local':
- No SYNCED or no REV: use `ecard-sync-prefer-local' as configured.
- REV newer than SYNCED: return nil (prefer server — it changed).
- REV older than or equal to SYNCED: return t (prefer local — server
  unchanged since last sync, so local edits take priority)."
  (let ((synced-time (ecard-sync--parse-org-timestamp synced-string))
        (rev-time (ecard-sync--parse-rev-timestamp rev-string)))
    (cond
     ((or (null synced-time) (null rev-time))
      ecard-sync-prefer-local)
     ;; REV newer than SYNCED: server changed since last sync
     ((time-less-p synced-time rev-time)
      nil)
     ;; REV older than or equal to SYNCED: server unchanged
     (t t))))

(defun ecard-sync--merge-ecards (local-vc server-vc)
  "Merge LOCAL-VC and SERVER-VC.
Returns new ecard object with merged properties.

When `ecard-sync-prefer-local' is non-nil, local is authoritative:
local values are always used for mapped properties.  Server values
are used only for unmapped data (extended properties).

When nil, the server is authoritative with the same logic reversed."
  (let ((merged (ecard)))

    ;; Set VERSION:4.0
    (setf (ecard-version merged)
          (list (ecard-property :name "VERSION" :value "4.0")))

    ;; For mapped properties, the preferred side is authoritative.
    ;; This ensures that local deletions propagate to the server
    ;; (and vice versa when prefer-server is set).
    (dolist (slot '(fn n email tel adr org note bday geo categories uid))
      (let* ((local-val (ecard--slot-value local-vc slot))
             (server-val (ecard--slot-value server-vc slot))
             ;; Normalize empty values to nil
             (local-normalized (if (ecard-sync--slot-empty-p local-val) nil local-val))
             (server-normalized (if (ecard-sync--slot-empty-p server-val) nil server-val))
             (value (if ecard-sync-prefer-local local-normalized server-normalized)))
        (when value
          (ecard--set-slot-value merged slot value))))

    ;; Always use server's extended properties if present, otherwise local
    (let ((server-ext (ecard-extended server-vc))
          (local-ext (ecard-extended local-vc)))
      (setf (ecard-extended merged) (or server-ext local-ext)))

    ;; Set new REV timestamp
    (ecard-set-property merged 'rev (format-time-string "%Y%m%dT%H%M%SZ" nil t))

    merged))

;;; Sync command

;;;###autoload
(defun ecard-sync-entry ()
  "Synchronize current org-contacts entry with CardDAV server.
Process:
1. Check if org :ID exists on server
2. If found on server: merge bidirectionally
3. If not found: search by FN (formatted name)
   - If found by FN: adopt server's UID (change org :ID to match)
   - If not found: use org :ID to create new vCard on server
4. Upload merged vCard to server

The org :ID property is used directly as the vCard UID.
If a vCard is found on the server by name search, the org :ID
is changed to match the server's UID."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in org-mode buffer"))

  (save-excursion
    (org-back-to-heading t)
    (let* ((addressbook (ecard-sync--get-addressbook))
           (local-vc (ecard-sync-org-to-ecard))
           (local-fn (ecard-get-property-value local-vc 'fn))
           (org-id (org-entry-get (point) "ID"))
           (synced-string (org-entry-get (point) "SYNCED"))
           (server-vc nil)
           (server-resource nil)
           (uid nil)
           (update-org-id nil)
           (action nil))

      (cond
       ;; Case 1: Has org ID - try to find on server by ID
       ((and org-id (not (string-empty-p org-id)))
        (setq uid org-id)
        (let ((match (ecard-sync--find-resource-by-uid addressbook uid)))
          (if match
              (progn
                (setq server-resource (car match))
                (setq server-vc (cdr match))
                (setq action "merged with server"))
            ;; Not found by ID - search by FN
            (let ((match (ecard-sync--find-resource-by-fn addressbook local-fn)))
              (if match
                  (progn
                    (setq server-resource (car match))
                    (setq server-vc (cdr match))
                    (setq uid (ecard-get-property-value server-vc 'uid))
                    (setq update-org-id t)
                    (setq action "found on server by name, adopted server UID"))
                (setq action "created on server with org ID"))))))

       ;; Case 2: No org ID - search by FN or generate new
       (t
        (let ((match (ecard-sync--find-resource-by-fn addressbook local-fn)))
          (if match
              (progn
                (setq server-resource (car match))
                (setq server-vc (cdr match))
                (setq uid (ecard-get-property-value server-vc 'uid))
                (setq update-org-id t)
                (setq action "found on server by name, adopted server UID"))
            (progn
              (setq uid (org-id-uuid))
              (setq update-org-id t)
              (setq action "created new vCard with generated UID"))))))

      ;; Determine merge direction from timestamps
      (let* ((server-rev (when server-vc
                           (ecard-get-property-value server-vc 'rev)))
             (ecard-sync-prefer-local
              (ecard-sync--determine-prefer-local synced-string server-rev))
             (final-vc (if server-vc
                           (ecard-sync--merge-ecards local-vc server-vc)
                         local-vc)))

        ;; Ensure UID is set
        (ecard-set-property final-vc 'uid uid)

        ;; Upload to server unconditionally — the SYNCED/REV comparison
        ;; has already determined the correct merge direction.
        ;; Preserve the original line ending style; new cards use CRLF.
        (let ((path (if server-resource
                        (oref server-resource url)
                      (concat uid ".vcf")))
              (le (if server-resource
                      (oref server-resource line-endings)
                    'crlf)))
          (ecard-carddav-put-ecard addressbook path final-vc nil le))

        ;; Update org entry
        (when update-org-id
          (org-set-property "ID" uid))
        (ecard-sync-ecard-to-org final-vc)

        ;; Record sync timestamp (org-only, not sent to server)
        (org-back-to-heading t)
        (org-set-property "SYNCED"
                          (format-time-string "[%Y-%m-%d %a %H:%M]"))

        (message "Synced \"%s\": %s" local-fn action)))))

;;;###autoload
(defun ecard-sync-all-entries ()
  "Synchronize all org-contacts entries in current buffer."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in org-mode buffer"))

  (save-excursion
    (goto-char (point-min))
    (let ((count 0)
          (errors 0))
      (while (re-search-forward org-heading-regexp nil t)
        (condition-case err
            (progn
              (ecard-sync-entry)
              (setq count (1+ count)))
          (error
           (setq errors (1+ errors))
           (message "Error syncing entry: %s" (error-message-string err)))))
      (message "Synced %d entries (%d errors)" count errors))))

(provide 'ecard-sync)
;;; ecard-sync.el ends here
