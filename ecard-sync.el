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
;; - NOTE: :NOTE property
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
  "Clear the addressbook cache.
Call this if server configuration changes."
  (interactive)
  (setq ecard-sync--addressbook-cache nil)
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
           (email (cdr (assoc "EMAIL" props)))
           (email2 (cdr (assoc "EMAIL2" props)))
           (phone (cdr (assoc "PHONE" props)))
           (address (cdr (assoc "ADDRESS" props)))
           (org-name (cdr (assoc "ORG" props)))
           (note (cdr (assoc "NOTE" props)))
           (birthday (cdr (assoc "BIRTHDAY" props)))
           (location (cdr (assoc "LOCATION" props)))
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
        (org-set-property "EMAIL" (car emails))
        (when (> (length emails) 1)
          (org-set-property "EMAIL2" (cadr emails))))

      ;; Set PHONE
      (when tel
        (org-set-property "PHONE" tel))

      ;; Set ADDRESS
      (when adr
        (let ((adr-str (if (listp adr)
                           (mapconcat #'identity adr ";")
                         adr)))
          (org-set-property "ADDRESS" adr-str)))

      ;; Set ORG
      (when org-name
        (let ((org-str (if (listp org-name)
                           (car org-name)
                         org-name)))
          (org-set-property "ORG" org-str)))

      ;; Set NOTE
      (when note
        (org-set-property "NOTE" note))

      ;; Set BIRTHDAY
      (when bday
        (org-set-property "BIRTHDAY" bday))

      ;; Set LOCATION from GEO
      (when geo
        (let ((location (ecard-sync--format-location geo)))
          (when location
            (org-set-property "LOCATION" location))))

      ;; Set tags from CATEGORIES
      (when (and categories (listp categories))
        (org-set-tags categories)))))

;;; Server operations

(defun ecard-sync--find-resource-by-uid (addressbook uid)
  "Find resource in ADDRESSBOOK with matching UID.
Returns resource object or nil."
  (let ((resources (oref addressbook resources)))
    ;; If resources not loaded, list them first
    (unless resources
      (setq resources (ecard-carddav-list-resources addressbook)))
    ;; Look for matching UID by path (many servers use UID.vcf)
    (let ((uid-path (concat uid ".vcf")))
      (cl-find-if (lambda (r)
                    (string-suffix-p uid-path (oref r path)))
                  resources))))

(defun ecard-sync--find-resource-by-fn (addressbook fn)
  "Find resource in ADDRESSBOOK with matching formatted name FN.
Returns cons (resource . ecard) or nil.
This fetches vCards to check FN, so it's slower than UID lookup."
  (ecard-sync--debug "Searching for FN=%s" fn)
  (let ((resources (oref addressbook resources)))
    (unless resources
      (setq resources (ecard-carddav-list-resources addressbook)))
    (catch 'found
      (dolist (resource resources)
        (condition-case nil
            (let* ((fetched (ecard-carddav-get-resource addressbook (oref resource url)))
                   (vc (oref fetched ecard))
                   (server-fn (ecard-get-property-value vc 'fn)))
              (when (and server-fn (string= (downcase server-fn) (downcase fn)))
                (throw 'found (cons fetched vc))))
          (error nil)))
      nil)))

;;; Merge logic

(defun ecard-sync--merge-ecards (local-vc server-vc)
  "Merge LOCAL-VC and SERVER-VC.
Returns new ecard object with merged properties.
Strategy: prefer local values when `ecard-sync-prefer-local' is t."
  (let ((merged (ecard)))

    ;; Set VERSION:4.0
    (setf (ecard-version merged)
          (list (ecard-property :name "VERSION" :value "4.0")))

    ;; Merge each property
    (dolist (slot '(fn n email tel adr org note bday geo categories uid))
      (let ((local-val (ecard--slot-value local-vc slot))
            (server-val (ecard--slot-value server-vc slot)))
        (cond
         ;; Both have values - prefer based on setting
         ((and local-val server-val)
          (ecard--set-slot-value merged slot
                                  (if ecard-sync-prefer-local local-val server-val)))
         ;; Only local has value
         (local-val
          (ecard--set-slot-value merged slot local-val))
         ;; Only server has value
         (server-val
          (ecard--set-slot-value merged slot server-val)))))

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
           (server-vc nil)
           (server-resource nil)
           (uid nil)
           (update-org-id nil)
           (action nil))

      (cond
       ;; Case 1: Has org ID - try to find on server by ID
       ((and org-id (not (string-empty-p org-id)))
        (setq uid org-id)
        (let ((resource (ecard-sync--find-resource-by-uid addressbook uid)))
          (if resource
              (progn
                (setq server-resource (ecard-carddav-get-resource addressbook (oref resource url)))
                (setq server-vc (oref server-resource ecard))
                (setq action "merged with server"))
            ;; Not found by ID - search by FN
            (let ((search-result (ecard-sync--find-resource-by-fn addressbook local-fn)))
              (if search-result
                  (progn
                    (setq server-resource (car search-result))
                    (setq server-vc (cdr search-result))
                    (setq uid (ecard-get-property-value server-vc 'uid))
                    (setq update-org-id t)
                    (setq action "found on server by name, adopted server UID"))
                (setq action "created on server with org ID"))))))

       ;; Case 2: No org ID - search by FN or generate new
       (t
        (let ((search-result (ecard-sync--find-resource-by-fn addressbook local-fn)))
          (if search-result
              (progn
                (setq server-resource (car search-result))
                (setq server-vc (cdr search-result))
                (setq uid (ecard-get-property-value server-vc 'uid))
                (setq update-org-id t)
                (setq action "found on server by name, adopted server UID"))
            (progn
              (setq uid (org-id-uuid))
              (setq update-org-id t)
              (setq action "created new vCard with generated UID"))))))

      ;; Merge if we have server data
      (let ((final-vc (if server-vc
                          (ecard-sync--merge-ecards local-vc server-vc)
                        local-vc)))

        ;; Ensure UID is set
        (ecard-set-property final-vc 'uid uid)

        ;; Upload to server
        (let ((path (concat uid ".vcf"))
              (etag (when server-resource (oref server-resource etag))))
          (ecard-carddav-put-ecard addressbook path final-vc etag))

        ;; Update org entry
        (when update-org-id
          (org-set-property "ID" uid))
        (ecard-sync-ecard-to-org final-vc)

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
