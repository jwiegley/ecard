# Emacs ecard

A complete vCard 4.0 (RFC 6350) parser and serializer for Emacs, providing a clean API for programmatic contact management.

## Features

- **Full vCard 4.0 Support** - Complete implementation of RFC 6350
- **Pure Elisp** - No external dependencies required
- **EIEIO-based Design** - Clean object-oriented API
- **Bidirectional** - Parse and serialize vCards with full fidelity
- **UTF-8 Support** - Proper handling of international characters
- **Extended Properties** - Support for X-* custom properties

## Why Use emacs-ecard?

- **Email Integration** - Parse vCard attachments from emails directly in Emacs
- **Contact Management** - Build custom contact management tools
- **Data Migration** - Convert between vCard and other formats (org-contacts, BBDB, CSV)
- **Automation** - Bulk operations on contact databases
- **Integration** - Add vCard support to existing Emacs packages

## Installation

### Manual Installation

1. Clone the repository:
```bash
git clone https://github.com/yourusername/emacs-ecard.git
```

2. Add to your Emacs configuration:
```elisp
(add-to-list 'load-path "/path/to/emacs-ecard")
(require 'ecard)
```

### Using use-package

```elisp
(use-package ecard
  :load-path "/path/to/emacs-ecard")
```

## Quick Start

### Parse a vCard from File

```elisp
(let ((contact (ecard-parse-file "~/contacts/john.vcf")))
  ;; Get the formatted name
  (ecard-get-property-value contact 'fn)
  ;; => "John Doe"

  ;; Get all email addresses
  (ecard-get-property-values contact 'email)
  ;; => ("john@example.com" "john.doe@work.com")

  ;; Get first phone number
  (ecard-get-property-value contact 'tel)
  ;; => "+1-555-1234")
```

### Create a New vCard

```elisp
(let ((contact (ecard-create
                :fn "Jane Smith"
                :n "Smith;Jane;Marie;Dr.;PhD"
                :email '("jane@example.com" "j.smith@university.edu")
                :tel '("+1-555-5678" "+1-555-8765")
                :org "Acme Corporation")))

  ;; Save to file
  (ecard-write-file contact "~/contacts/jane.vcf"))
```

### Parse from String

```elisp
(let* ((ecard-text "BEGIN:VCARD
VERSION:4.0
FN:John Doe
EMAIL:john@example.com
END:VCARD")
       (contact (ecard-parse ecard-text)))
  (ecard-get-property-value contact 'email))
;; => "john@example.com"
```

### Modify Properties

```elisp
(let ((contact (ecard-parse-file "~/contacts/john.vcf")))
  ;; Replace all emails
  (ecard-set-property contact 'email "newemail@example.com")

  ;; Add an additional phone number
  (ecard-add-property contact 'tel "+1-555-9999")

  ;; Update organization
  (ecard-set-property contact 'org "New Company Inc.")

  ;; Save changes
  (ecard-write-file contact "~/contacts/john-updated.vcf"))
```

## API Reference

### Parsing Functions

- `(ecard-parse STRING)` - Parse vCard string into ecard object
- `(ecard-parse-file FILENAME)` - Parse vCard from file
- `(ecard-parse-buffer)` - Parse current buffer as vCard

### Creation and Serialization

- `(ecard-create &rest PROPERTIES)` - Create new ecard with properties
- `(ecard-serialize VCARD)` - Convert ecard object to RFC 6350 string
- `(ecard-write-file VCARD FILENAME)` - Write ecard to file

### Property Access

- `(ecard-get-property-value VCARD PROPERTY)` - Get first value of property
- `(ecard-get-property-values VCARD PROPERTY)` - Get all values of property
- `(ecard-set-property VCARD PROPERTY VALUE)` - Replace property value(s)
- `(ecard-add-property VCARD PROPERTY VALUE)` - Add additional property value

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
               (ecard-get-property-values contact 'email)))
   (mapcar #'ecard-parse-file vcf-files)))
```

### Export to CSV

```elisp
(defun ecard-to-csv (ecard)
  "Convert vCard to CSV row."
  (mapconcat #'identity
             (list (or (ecard-get-property-value ecard 'fn) "")
                   (or (ecard-get-property-value ecard 'email) "")
                   (or (ecard-get-property-value ecard 'tel) "")
                   (or (ecard-get-property-value ecard 'org) ""))
             ","))
```

### Bulk Update Organization

```elisp
(defun update-organization (directory old-name new-name)
  "Update organization name in all vCards in DIRECTORY."
  (dolist (file (directory-files directory t "\\.vcf$"))
    (let ((contact (ecard-parse-file file)))
      (when (equal (ecard-get-property-value contact 'org) old-name)
        (ecard-set-property contact 'org new-name)
        (ecard-write-file contact file)))))
```

## Current Limitations

- **vCard 4.0 Only** - No support for VERSION 3.0 vCards
- **Single vCard Parsing** - Cannot parse multiple vCards in one file
- **No Value Validation** - Dates, URIs, etc. are not validated
- **No MIME Handling** - PHOTO/LOGO/SOUND properties store raw strings
- **Limited High-Level API** - No convenience methods like `ecard-add-email` yet

## Testing

The ecard project has **comprehensive test coverage** with **395+ tests** covering:
- RFC 6350 vCard 4.0 compliance (113 tests)
- CardDAV protocol (RFC 6352) (52 tests)
- org-contacts integration (89 tests)
- BBDB compatibility and migration (67 tests)
- Display, tools, and utilities (74 tests)

### Quick Start

```bash
# Using Eask (recommended)
eask install-deps
eask run script test              # Run all tests
eask run script test-quick        # Quick smoke tests
eask run script test-compliance   # RFC compliance tests

# Using Emacs directly
emacs -batch -L . -l ecard.el -l ecard-test.el \
  -f ert-run-tests-batch-and-exit
```

### Test Infrastructure

The project includes:
- **Custom test framework** with assertion macros and test data generators
- **RFC compliance testing** with automated requirement tracking
- **Performance benchmarks** and security validation
- **CI/CD integration** with GitHub Actions
- **Pre-commit hooks** for quick validation

## Contributing

Contributions are welcome! Please ensure:
- All tests pass
- New features include tests
- Code follows existing style
- Changes are documented

## License

GPL3

## See Also

- [RFC 6350](https://tools.ietf.org/html/rfc6350) - vCard Format Specification
- [org-contacts](https://github.com/girzel/org-contacts) - Org-mode contact management
- [BBDB](https://www.emacswiki.org/emacs/BbdbMode) - Big Brother Database
