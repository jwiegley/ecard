# emacs-vcard

A complete vCard 4.0 (RFC 6350) parser and serializer for Emacs, providing a clean API for programmatic contact management.

## Features

- **Full vCard 4.0 Support** - Complete implementation of RFC 6350
- **Pure Elisp** - No external dependencies required
- **EIEIO-based Design** - Clean object-oriented API
- **Bidirectional** - Parse and serialize vCards with full fidelity
- **UTF-8 Support** - Proper handling of international characters
- **Extended Properties** - Support for X-* custom properties

## Why Use emacs-vcard?

- **Email Integration** - Parse vCard attachments from emails directly in Emacs
- **Contact Management** - Build custom contact management tools
- **Data Migration** - Convert between vCard and other formats (org-contacts, BBDB, CSV)
- **Automation** - Bulk operations on contact databases
- **Integration** - Add vCard support to existing Emacs packages

## Installation

### Manual Installation

1. Clone the repository:
```bash
git clone https://github.com/yourusername/emacs-vcard.git
```

2. Add to your Emacs configuration:
```elisp
(add-to-list 'load-path "/path/to/emacs-vcard")
(require 'vcard)
```

### Using use-package

```elisp
(use-package vcard
  :load-path "/path/to/emacs-vcard")
```

## Quick Start

### Parse a vCard from File

```elisp
(let ((contact (vcard-parse-file "~/contacts/john.vcf")))
  ;; Get the formatted name
  (vcard-get-property-value contact 'fn)
  ;; => "John Doe"

  ;; Get all email addresses
  (vcard-get-property-values contact 'email)
  ;; => ("john@example.com" "john.doe@work.com")

  ;; Get first phone number
  (vcard-get-property-value contact 'tel)
  ;; => "+1-555-1234")
```

### Create a New vCard

```elisp
(let ((contact (vcard-create
                :fn "Jane Smith"
                :n "Smith;Jane;Marie;Dr.;PhD"
                :email '("jane@example.com" "j.smith@university.edu")
                :tel '("+1-555-5678" "+1-555-8765")
                :org "Acme Corporation")))

  ;; Save to file
  (vcard-write-file contact "~/contacts/jane.vcf"))
```

### Parse from String

```elisp
(let* ((vcard-text "BEGIN:VCARD
VERSION:4.0
FN:John Doe
EMAIL:john@example.com
END:VCARD")
       (contact (vcard-parse vcard-text)))
  (vcard-get-property-value contact 'email))
;; => "john@example.com"
```

### Modify Properties

```elisp
(let ((contact (vcard-parse-file "~/contacts/john.vcf")))
  ;; Replace all emails
  (vcard-set-property contact 'email "newemail@example.com")

  ;; Add an additional phone number
  (vcard-add-property contact 'tel "+1-555-9999")

  ;; Update organization
  (vcard-set-property contact 'org "New Company Inc.")

  ;; Save changes
  (vcard-write-file contact "~/contacts/john-updated.vcf"))
```

## API Reference

### Parsing Functions

- `(vcard-parse STRING)` - Parse vCard string into vcard object
- `(vcard-parse-file FILENAME)` - Parse vCard from file
- `(vcard-parse-buffer)` - Parse current buffer as vCard

### Creation and Serialization

- `(vcard-create &rest PROPERTIES)` - Create new vcard with properties
- `(vcard-serialize VCARD)` - Convert vcard object to RFC 6350 string
- `(vcard-write-file VCARD FILENAME)` - Write vcard to file

### Property Access

- `(vcard-get-property-value VCARD PROPERTY)` - Get first value of property
- `(vcard-get-property-values VCARD PROPERTY)` - Get all values of property
- `(vcard-set-property VCARD PROPERTY VALUE)` - Replace property value(s)
- `(vcard-add-property VCARD PROPERTY VALUE)` - Add additional property value

### Properties Supported

All standard vCard 4.0 properties are supported:

- **Identification**: `fn`, `n`, `nickname`, `photo`, `bday`, `anniversary`, `gender`
- **Contact**: `tel`, `email`, `impp`, `lang`
- **Address**: `adr`
- **Organization**: `title`, `role`, `logo`, `org`, `member`, `related`
- **Web**: `url`, `key`
- **Calendar**: `fburl`, `caladruri`, `caluri`
- **Metadata**: `version`, `prodid`, `rev`, `sound`, `uid`, `kind`
- **Notes**: `note`, `categories`
- **Time Zone**: `tz`
- **Geographic**: `geo`
- **Extended**: Any `X-*` properties

## Examples

### Filter Contacts by Email Domain

```elisp
(defun contacts-from-domain (vcf-files domain)
  "Return contacts with email addresses from DOMAIN."
  (seq-filter
   (lambda (contact)
     (seq-some (lambda (email)
                 (string-match-p (concat "@" domain "$") email))
               (vcard-get-property-values contact 'email)))
   (mapcar #'vcard-parse-file vcf-files)))
```

### Export to CSV

```elisp
(defun vcard-to-csv (vcard)
  "Convert vCard to CSV row."
  (mapconcat #'identity
             (list (or (vcard-get-property-value vcard 'fn) "")
                   (or (vcard-get-property-value vcard 'email) "")
                   (or (vcard-get-property-value vcard 'tel) "")
                   (or (vcard-get-property-value vcard 'org) ""))
             ","))
```

### Bulk Update Organization

```elisp
(defun update-organization (directory old-name new-name)
  "Update organization name in all vCards in DIRECTORY."
  (dolist (file (directory-files directory t "\\.vcf$"))
    (let ((contact (vcard-parse-file file)))
      (when (equal (vcard-get-property-value contact 'org) old-name)
        (vcard-set-property contact 'org new-name)
        (vcard-write-file contact file)))))
```

## Current Limitations

- **vCard 4.0 Only** - No support for VERSION 3.0 vCards
- **Single vCard Parsing** - Cannot parse multiple vCards in one file
- **No Value Validation** - Dates, URIs, etc. are not validated
- **No MIME Handling** - PHOTO/LOGO/SOUND properties store raw strings
- **Limited High-Level API** - No convenience methods like `vcard-add-email` yet

## Testing

Run the test suite:

```bash
# All tests
emacs -batch -L . -l vcard.el -l vcard-test.el -f ert-run-tests-batch-and-exit

# Interactive testing (for debugging)
emacs -l vcard.el -l vcard-test.el
# Then: M-x ert RET t RET

# Single test
emacs -batch -L . -l vcard.el -l vcard-test.el \
  --eval "(ert-run-tests-batch-and-exit 'vcard-parse-simple-test)"
```

## Contributing

Contributions are welcome! Please ensure:
- All tests pass
- New features include tests
- Code follows existing style
- Changes are documented

## License

[Your chosen license]

## See Also

- [RFC 6350](https://tools.ietf.org/html/rfc6350) - vCard Format Specification
- [org-contacts](https://github.com/girzel/org-contacts) - Org-mode contact management
- [BBDB](https://www.emacswiki.org/emacs/BbdbMode) - Big Brother Database