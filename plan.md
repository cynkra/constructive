# PLAN 🔄

Project: construct_serialize() - Complete R Object Serialization Support
Feature to convert any R object to constructive code via its serialized binary representation.

## Current Status
- Basic framework implemented (construct_serialize.R, serialize_header.R, serialize_data.R)
- Header serialization: Complete (all header components working)
- Data serialization: STRSXP (character vectors) and CHARSXP (single strings) fully implemented and tested
- Character types complete with NA_character_ support
- Test suite created (tests/testthat/test-construct_serialize.R) - 13 tests passing
- Feature branch: f-635-construct_serialize
- Latest: Fixed NA_character_ handling, added comprehensive tests

## 1. Core Framework ✅

The main construct_serialize() function and header processing infrastructure.

### 1.1 Main Function ✅
- ✅ construct_serialize() main dispatcher function
- ✅ Header processing with serialize_header()
- ✅ Data processing with serialize_data()
- ✅ Code generation and formatting

### 1.2 Header Serialization ✅
- ✅ serialize_serialization_format() - format byte
- ✅ serialize_header_separator() - 0x0a separator
- ✅ serialize_workspace_format() - workspace version
- ✅ serialize_r_version() - R version used
- ✅ serialize_minimum_version() - minimum R version
- ✅ serialize_character_set() - encoding information

### 1.3 Data Infrastructure ✅
- ✅ serialize_data() main dispatcher
- ✅ serialize_packed_header() - type header reader
- ✅ trim_last_comma() utility function

## 2. Character Types ✅

String handling is the foundation of serialization display.

### 2.1 Character Vectors (STRSXP, 0x10) ✅
- ✅ serialize_strsxp() implementation
- ✅ Vector length handling
- ✅ Multi-element iteration
- ✅ Multi-byte character support (UTF-8)

### 2.2 Single Strings (CHARSXP, 0x09) ✅
- ✅ serialize_chrsxp() implementation
- ✅ String length handling
- ✅ Character-by-character display with alignment
- ✅ Multi-byte character support
- ✅ NA_character_ handling (length = -1, 0xffffffff)

## 3. Initial Testing and Validation ✅

Verify current implementation works correctly before expanding to new types.

### 3.1 Test Character Types ✅
- ✅ Create test file tests/testthat/test-construct_serialize.R
- ✅ Test simple character vector: c("a", "b", "c")
- ✅ Test multi-byte characters: c("ab", "aé")
- ✅ Test empty character vector: character(0)
- ✅ Test single-element vector: "hello"
- ✅ Test NA_character_ values: c("a", NA, "b")
- ✅ Verify round-trip fidelity: eval(construct_serialize(x)) identical to x
- ✅ All 13 tests passing

## 4. Atomic Vector Types 🚧

Add support for the most common R data types after character vectors.

### 4.1 Logical Vectors (LGLSXP, 0x0A) 🟢
- 🟢 Implement serialize_lglsxp() function
- 🟢 Handle TRUE, FALSE, NA values (1, 0, -2147483648)
- 🟢 Add dispatcher case in serialize_data()
- 🟢 Add tests for logical vectors

### 4.2 Integer Vectors (INTSXP, 0x0D) 🚧
- 🚧 Implement serialize_intsxp() function
- 🚧 Handle NA_integer_ (-2147483648)
- 🚧 Add dispatcher case in serialize_data()
- 🚧 Add tests for integer vectors

### 4.3 Numeric Vectors (REALSXP, 0x0E) 🚧
- 🚧 Implement serialize_realsxp() function
- 🚧 Handle NA, NaN, Inf, -Inf (IEEE 754 doubles)
- 🚧 Add dispatcher case in serialize_data()
- 🚧 Add tests for numeric vectors

### 4.4 Complex Vectors (CPLXSXP, 0x0F) 🚧
- 🚧 Implement serialize_cplxsxp() function
- 🚧 Handle two doubles per complex number
- 🚧 Add dispatcher case in serialize_data()
- 🚧 Add tests for complex vectors

### 4.5 Raw Vectors (RAWSXP, 0x18) 🚧
- 🚧 Implement serialize_rawsxp() function
- 🚧 Add dispatcher case in serialize_data()
- 🚧 Add tests for raw vectors

## 5. NULL and Symbols 🚧

Basic building blocks for R expressions and attributes.

### 5.1 NULL Values (NILVALUE_SXP, 0xFE) 🚧
- 🚧 Implement serialize_nilvalue() function
- 🚧 Add dispatcher case in serialize_data()
- 🚧 Add tests for NULL

### 5.2 Symbols (SYMSXP, 0x01) 🚧
- 🚧 Implement serialize_symsxp() function
- 🚧 Add dispatcher case in serialize_data()
- 🚧 Add tests for symbols

## 6. List Types 🚧

Container types that hold other objects.

### 6.1 Generic Lists (VECSXP, 0x13) 🚧
- 🚧 Implement serialize_vecsxp() function (placeholder exists)
- 🚧 Handle recursive list structures
- 🚧 Add dispatcher case in serialize_data()
- 🚧 Add tests for lists

### 6.2 Pairlists (LISTSXP, 0x02) 🚧
- 🚧 Implement serialize_listsxp() function (placeholder exists)
- 🚧 Handle attributes (which use pairlists)
- 🚧 Handle CAR/CDR/TAG structure
- 🚧 Add dispatcher case in serialize_data()
- 🚧 Add tests for pairlists

## 7. Attributes Support 🚧

Objects can have attributes like names, class, dim, etc.

### 7.1 Attribute Handling 🚧
- 🚧 Detect and handle HAS_ATTR flag in packed header
- 🚧 Handle names attribute
- 🚧 Handle class attribute
- 🚧 Handle dim attribute (for matrices/arrays)
- 🚧 Handle dimnames attribute
- 🚧 Add tests for attributed objects

## 8. Advanced Types 🚧

More complex R objects.

### 8.1 Language Objects (LANGSXP, 0x06) 🚧
- 🚧 Implement serialize_langsxp() function
- 🚧 Add dispatcher case in serialize_data()
- 🚧 Add tests for language objects

### 8.2 Expression Vectors (EXPRSXP, 0x14) 🚧
- 🚧 Implement serialize_exprsxp() function
- 🚧 Add dispatcher case in serialize_data()
- 🚧 Add tests for expressions

### 8.3 Functions (CLOSXP, 0x03) 🚧
- 🚧 Implement serialize_closxp() function
- 🚧 Handle formals, body, environment
- 🚧 Add dispatcher case in serialize_data()
- 🚧 Add tests for functions

### 8.4 Environments (ENVSXP, 0x04) 🚧
- 🚧 Implement serialize_envsxp() function
- 🚧 Add dispatcher case in serialize_data()
- 🚧 Add tests for environments

### 8.5 S4 Objects (S4SXP, 0x19) 🚧
- 🚧 Implement serialize_s4sxp() function
- 🚧 Add dispatcher case in serialize_data()
- 🚧 Add tests for S4 objects

## 9. Special Cases 🚧

Handle references and special object types.

### 9.1 Reference Types 🚧
- 🚧 Handle REFSXP (0xFF) for shared objects
- 🚧 Track and reuse references
- 🚧 Add tests for objects with references

### 9.2 External Pointers and Builtins 🚧
- 🚧 Handle EXTPTRSXP (0x16)
- 🚧 Handle SPECIALSXP (0x07) and BUILTINSXP (0x08)
- 🚧 Document limitations for non-serializable types

## 10. Documentation and Integration 🚧

### 10.1 Documentation 🚧
- 🚧 Add comprehensive examples to construct_serialize roxygen
- 🚧 Document supported and unsupported types
- 🚧 Add usage notes about when to use construct_serialize

### 10.2 Integration 🚧
- 🚧 Ensure construct_serialize is exported in NAMESPACE
- 🚧 Update package documentation
- 🚧 Consider adding to main package README

## References
- R Internals: https://cran.r-project.org/doc/manuals/r-release/R-ints.html#Serialization-Formats
- Serialization format: [header: 14 bytes] [data: recursive type headers + content]
- Strategy: Build incrementally, test each type before proceeding
