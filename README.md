# ecard

I've been managing contacts in Emacs for years now -- org-contacts, BBDB,
various custom hacks -- and kept running into the same wall: there's no proper
vCard 4.0 library for Emacs. Every tool I found was either stuck on vCard 3.0,
couldn't round-trip without mangling data, or handled UTF-8 badly. So I wrote
one.

ecard is a complete vCard 4.0 (RFC 6350) parser and serializer for Emacs. It
uses `cl-defstruct` for the data model, handles line folding at 75 *octets* (not
characters, which matters for multi-byte text), and can parse and serialize
vCards with full fidelity. No external dependencies -- just Emacs 30.1+.

## Quick start

Parse a vCard from a file:

```elisp
(let ((contact (ecard-parse-file "~/contacts/john.vcf")))
  (ecard-get-property-value contact 'fn)    ;; => "John Doe"
  (ecard-get-property-values contact 'email) ;; => ("john@example.com" "john@work.com")
  (ecard-get-property-value contact 'tel))   ;; => "+1-555-1234"
```

Create a new vCard:

```elisp
(let ((contact (ecard-create
                :fn "Jane Smith"
                :n "Smith;Jane;Marie;Dr.;PhD"
                :email '("jane@example.com" "j.smith@university.edu")
                :tel '("+1-555-5678")
                :org "Acme Corporation")))
  (ecard-write-file contact "~/contacts/jane.vcf"))
```

Parse from a string:

```elisp
(ecard-parse "BEGIN:VCARD\nVERSION:4.0\nFN:John Doe\nEMAIL:john@example.com\nEND:VCARD")
```

## Installation

Clone the repo and add it to your load-path:

```elisp
(add-to-list 'load-path "/path/to/ecard")
(require 'ecard)
```

Or with `use-package`:

```elisp
(use-package ecard
  :load-path "/path/to/ecard")
```

## What's included

The core library (`ecard.el`) handles parsing, serialization, and the data
model. On top of that:

- **ecard-compat** -- vCard 3.0 to 4.0 conversion, so you can import older
  contact files
- **ecard-carddav** -- CardDAV protocol support (RFC 6352) for syncing with
  remote servers
- **ecard-org** -- bidirectional conversion between vCards and org-contacts
- **ecard-display** -- rendering vCards in Emacs buffers
- **ecard-sync** -- two-way sync engine with conflict resolution
- **ecard-tools** -- bulk operations, filtering, merging, deduplication
- **ecard-widget** -- Emacs widget interface for editing contacts

## API

### Parsing and serialization

- `(ecard-parse STRING)` -- parse a vCard string into an ecard struct
- `(ecard-parse-file FILENAME)` -- parse a .vcf file
- `(ecard-parse-buffer)` -- parse the current buffer
- `(ecard-create &rest PROPERTIES)` -- create a new ecard
- `(ecard-serialize VCARD)` -- convert an ecard to an RFC 6350 string
- `(ecard-write-file VCARD FILENAME)` -- write to a .vcf file

### Property access

- `(ecard-get-property-value VCARD PROP)` -- first value of a property
- `(ecard-get-property-values VCARD PROP)` -- all values of a property
- `(ecard-set-property VCARD PROP VALUE)` -- replace a property
- `(ecard-add-property VCARD PROP VALUE)` -- append a property value

### Supported properties

All standard vCard 4.0 properties: `fn`, `n`, `nickname`, `photo`, `bday`,
`anniversary`, `gender`, `tel`, `email`, `impp`, `lang`, `adr`, `title`,
`role`, `logo`, `org`, `member`, `related`, `url`, `key`, `fburl`,
`caladruri`, `caluri`, `version`, `prodid`, `rev`, `sound`, `uid`, `kind`,
`note`, `categories`, `tz`, `geo`, plus any `X-*` extended properties.

## Development

The project uses Nix for reproducible builds and a full development shell:

```bash
nix develop          # enter dev shell with Emacs, package-lint, lefthook
nix build            # byte-compile the package (warnings are errors)
nix flake check      # run all checks: tests, lint, format, coverage, perf, fuzz
```

Or run individual checks directly:

```bash
./scripts/test.sh           # ERT test suite
./scripts/byte-compile.sh   # byte-compile with warnings-as-errors
./scripts/lint.sh           # package-lint + checkdoc
./scripts/check-format.sh   # indentation check
./scripts/coverage.sh       # test coverage (60% minimum)
./scripts/perf-check.sh     # performance benchmark regression check
./scripts/fuzz-test.sh      # fuzz the parser with random inputs
```

Pre-commit hooks run all checks in parallel via [lefthook](https://github.com/evilmartians/lefthook):

```bash
lefthook install     # set up git hooks
```

To set a performance baseline (for the 5% regression check):

```bash
./scripts/update-perf-baseline.sh
```

## Current limitations

- **vCard 4.0 only** -- the parser rejects VERSION:3.0 (use `ecard-compat` to
  convert older files first)
- **Single vCard per parse call** -- use `ecard-compat-parse-multiple` for files
  with multiple vCards
- **No value type validation** -- dates, URIs, etc. aren't validated against
  their expected formats
- **No MIME handling** -- PHOTO/LOGO/SOUND are stored as strings, not decoded

## Testing

The test suite has 1290+ ERT tests covering RFC 6350 compliance, CardDAV
protocol, org-contacts integration, BBDB compatibility, display rendering, and
more. Tests run in CI via GitHub Actions on every push and pull request.

## License

BSD 3-Clause. See [LICENSE.md](LICENSE.md).
