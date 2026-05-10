# Constructive Package Structure

## Overview

This directory contains extracted sub-packages from the constructive ecosystem.

## constructive.core

The `constructive.core` package contains the core infrastructure for code generation:

### Included Components

#### Infrastructure Files (34 files)
- Core functions: `construct.R`, `construct_*.R` (clip, diff, dput, dump, reprex, signature)
- Helper utilities: `construct-helpers.R`, `character-helpers.R`, `deparse_*.R`, `format_atomic.R`
- Options system: `opts.R`, `global-options.R`
- Attribute handling: `repair_attributes.R`, `contains_self_reference.R`
- Templates: `templates.R`, `custom_constructors.R`
- Package infrastructure: `constructive-package.R`, `zzz.R`, `00_meta.R`
- Error handling: `abort.R`, `bypass.R`
- Other utilities: `utils.R`, `environment_utils.R`, `s3_register.R`, `roxygen2-tags.R`
- Documentation: `document-other-opts.R`, `expect_construct.R`
- Lifecycle management: `import-standalone-lifecycle.R`

#### Base Type Methods (18 files)
Files implementing constructors for base R types:
- `s3-NULL.R` - NULL type
- `s3-atomic.R` - Base atomic vectors
- `s3-character.R` - Character vectors
- `s3-integer.R` - Integer vectors
- `s3-double.R` - Double/numeric vectors
- `s3-complex.R` - Complex numbers
- `s3-logical.R` - Logical vectors
- `s3-raw.R` - Raw vectors
- `s3-list.R` - Lists
- `s3-array.R` - Arrays
- `s3-matrix.R` - Matrices
- `s3-environment.R` - Environments
- `s3-function.R` - Functions
- `s3-language.R` - Language objects (calls, expressions)
- `s3-pairlist.R` - Pairlists
- `s3-dots.R` - ... (dots)
- `s3-externalptr.R` - External pointers
- `s3-object.R` - Object type

#### S4 Support
- `s4.R` - S4 object construction

#### C++ Code
- `src/constructive.cpp` - C functions for external pointer handling and environment retrieval

#### Tests
26 test files covering core functionality:
- Infrastructure tests: `test-construct-*.R`, `test-deparse_call.R`, `test-opts.R`, etc.
- Type tests: `test-s3-array.R`, `test-s3-list.R`, `test-s3-environment.R`, etc.
- Utility tests: `test-utils.R`, `test-repair_attributes.R`, etc.

### Purpose

The `constructive.core` package serves as a lightweight, low-dependency foundation that:
1. Provides the default `.cstr_construct()` method
2. Handles all base R types
3. Supplies infrastructure functions used by extension packages
4. Can be independently installed and tested

### Dependencies

Imports:
- cli (>= 3.1.0)
- diffobj
- methods
- rlang (>= 1.0.0)
- waldo

## Future Packages

Additional packages will be extracted in future iterations:
- `constructive.base` - Base R class methods (Date, POSIXct, factor, data.frame, etc.)
- `constructive.ggplot2` - ggplot2 constructors
- `constructive.methods` - Other package-specific methods

## Notes

Currently, the main `constructive` package still contains all code. The extraction to `constructive.core` is the first step in the modularization process. Future work will:
1. Update the main package to import from `constructive.core`
2. Remove duplicated code from the main package
3. Extract additional specialized packages
