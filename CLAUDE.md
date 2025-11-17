# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

Complete vCard 4.0 (RFC 6350) parser and serializer library for Emacs using EIEIO object-oriented design. This is a standalone library with no external dependencies, providing a programmatic API for vCard manipulation.

Review the file @VCARD_ANALYSIS.md to understand the architecture and design of this package.

## Key Commands

```bash
# Run all tests
emacs -batch -L . -l vcard.el -l vcard-test.el -f ert-run-tests-batch-and-exit

# Run tests interactively (to debug specific failures)
emacs -l vcard.el -l vcard-test.el
# Then: M-x ert RET t RET

# Run single test
emacs -batch -L . -l vcard.el -l vcard-test.el --eval "(ert-run-tests-batch-and-exit 'vcard-parse-simple-test)"

# Check for byte-compilation warnings
emacs -batch -L . -f batch-byte-compile vcard.el
```

## High-Level Architecture

### EIEIO Class Hierarchy

The package uses two EIEIO classes to represent vCards:

1. **`vcard-property`** (vcard.el:62-83) - Individual property with group, name, parameters, and value
2. **`vcard`** (vcard.el:85-261) - Complete vCard with 30+ RFC 6350 property slots plus `:extended` for X-* properties

Each property type in the vcard class stores a list of `vcard-property` objects, allowing multiple instances (e.g., multiple emails). The `:extended` slot uses an alist structure for arbitrary X-* properties.

### Data Processing Pipeline

```
Parsing:   Text → Line Unfolding → Property Parsing → Value Unescaping → vcard object
Serialize: vcard object → Value Escaping → Line Folding (75 octets) → RFC-compliant text
```

Key design decision: Line folding counts UTF-8 **octets** not characters (vcard.el:407-435), ensuring RFC 6350 compliance without breaking multi-byte characters.

## Current Limitations from README

1. **Single vCard Parsing** - Parser handles one vCard per call, not multiple (vcard-README.md:94-97)
2. **No VERSION 3.0 Support** - Only vCard 4.0 supported (vcard-README.md:99-102)
3. **No Value Type Validation** - Dates, URIs not validated (vcard-README.md:104-107)
4. **No MIME Type Handling** - PHOTO/LOGO/SOUND are strings only, no base64 (vcard-README.md:109-112)
5. **No Property-Specific Methods** - Missing helpers like `vcard-add-email` (vcard-README.md:114-118)

## API Reference

### Public Functions (all in vcard.el)

- `vcard-parse` (line 468) - Parse string to vcard object
- `vcard-parse-file` (line 514) - Parse file to vcard object
- `vcard-parse-buffer` (line 522) - Parse buffer to vcard object
- `vcard-serialize` (line 528) - Serialize vcard to string
- `vcard-write-file` (line 555) - Write vcard to file
- `vcard-create` (line 563) - Create vcard with keyword args
- `vcard-get-property-value` (line 700) - Get first property value
- `vcard-get-property-values` (line 693) - Get all property values
- `vcard-set-property` (line 708) - Replace property
- `vcard-add-property` (line 722) - Append property

### Error Conditions

- `vcard-parse-error` - Malformed vCard syntax
- `vcard-validation-error` - Missing VERSION or FN property

## Critical Design Notes

1. **vCard 4.0 ONLY** - Parser rejects VERSION:3.0. Must be exactly "4.0" (vcard.el:504-509)
2. **Extended Properties** - X-* properties stored in `:extended` slot as alist, not as direct slots
3. **UTF-8 Octets** - Line folding at 75 **octets** not characters (vcard.el:407-435)
4. **Parameters Uppercase** - All parameter names converted to uppercase on parse (vcard.el:328)
5. **No Interactive Commands** - Pure library, no user-facing commands yet

## Missing Infrastructure

The package currently lacks:
- Build system (no Eask, Make, or Cask)
- CI/CD configuration
- Interactive commands for users
- Integration with BBDB/org-contacts

## Test Coverage

14 tests in vcard-test.el covering:
- Basic parsing and serialization
- Line folding/unfolding with UTF-8
- Extended properties (X-*)
- Validation errors
- Round-trip fidelity
- File I/O

Tests pass in ~4ms. No tests for malformed input or performance with large vCards.
