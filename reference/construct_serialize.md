# Build code to recreate an object from its serialized form

`construct_serialize()` creates code that reconstructs an R object by
serializing it to raw bytes and then calling
[`unserialize()`](https://rdrr.io/r/base/serialize.html). This is a
fallback method that works for almost any R object, including those that
are difficult or impossible to construct idiomatically.

## Usage

``` r
construct_serialize(x, collapse_header = FALSE)
```

## Arguments

- x:

  An R object to serialize and reconstruct.

- collapse_header:

  Logical. If `TRUE`, the serialization header is displayed as a single
  collapsed line (`c(0x58, 0x0a, ..., 0x38)`). If `FALSE` (default), the
  header is displayed with detailed comments explaining each component.

## Value

An object of class `"constructive_code"` containing the generated code
as a character vector. Each element is one line of code.

## Details

This function generates code in the form:

    unserialize(as.raw(c(
      # --- HEADER ---
      c(0x58, 0x0a, ...),
      # --- DATA ---
      c(0x00, 0x00, ...)
    )))

The generated code includes detailed comments explaining the binary
structure, making it useful for understanding R's serialization format.

### Limitations

`construct_serialize()` reproduces faithfully what
[`serialize()`](https://rdrr.io/r/base/serialize.html) does, so it
shares its limitations. Some objects don't survive serialization or can
be restored only in some conditions:

- **External pointers**: The address is lost, we get a null pointer

- **Weak references**: The key and value are lost

- **Connections**: Only the connection number is stored, the
  reconstructed object is valid only in the same session, as long as the
  connection is open

- **Namespaces and package environments**: They are stored by name, so
  the package must be available when the code is run

The function is designed for small objects, every byte is documented so
the output is long and large objects (hundreds of kilobytes once
serialized) are slow to process. Note that functions defined in a file
keep a reference to the whole file through their srcref, use
[`utils::removeSource()`](https://rdrr.io/r/utils/removeSource.html) to
get rid of it.

## See also

[`construct()`](https://cynkra.github.io/constructive/reference/construct.md)
for idiomatic object construction,
[`serialize()`](https://rdrr.io/r/base/serialize.html) for the
underlying serialization mechanism.

## Examples

``` r
# Simple objects
construct_serialize(1:5)
#> unserialize(as.raw(c(
#>   # --- HEADER ---
#>   c(
#>     # 1: serialization format, A for ASCII, X for binary, rawToChar(as.raw(0x58)) == 'X'
#>     0x58,
#>     # 2: always 0x0a, the bytecode for "\n", just a separator
#>     0x0a,
#>     # 3-6: The workspace format version, 3 since R 3.5.0, always 2 before that
#>     0x00, 0x00, 0x00, 0x03,
#>     # 7-10: The R version used to serialize, here 4.6.1
#>     0x00, 0x04, 0x06, 0x01,
#>     # 11-14: The minimal R version that can unserialize this, e.g. 3.5.0
#>     0x00, 0x03, 0x05, 0x00,
#>     # 15-18: The length of the character set name in bytes: 5
#>     0x00, 0x00, 0x00, 0x05,
#>     # 19-23: Which character set is used for the encoding, e.g. 'UTF-8'
#>     #  U     T     F     -     8
#>     0x55, 0x54, 0x46, 0x2d, 0x38
#>   ),
#>   # --- DATA ---
#>   c(
#>     # 24-27: ALTREP_SXP (alternative representation)
#>     0x00, 0x00, 0x00, 0xee,
#>     # 28: ALTREP class info (pairlist)
#>     # 28-31: LISTSXP (pairlist)
#>     0x00, 0x00, 0x00, 0x02,
#>     # 32: LISTSXP CAR (value)
#>     # 32-35: SYMSXP (symbol)
#>     0x00, 0x00, 0x00, 0x01,
#>     # 36-39: CHARSXP (string) | flags: ASCII
#>     0x00, 0x04, 0x00, 0x09,
#>     # 40-43: length of string in bytes: 14
#>     0x00, 0x00, 0x00, 0x0e,
#>     #  c     o     m     p     a     c     t     _     i     n     t     s     
#>     0x63, 0x6f, 0x6d, 0x70, 0x61, 0x63, 0x74, 0x5f, 0x69, 0x6e, 0x74, 0x73,
#>     #  e     q   
#>     0x65, 0x71,
#>     # 58: LISTSXP CDR (next node or NULL)
#>     # 58-61: LISTSXP (pairlist)
#>     0x00, 0x00, 0x00, 0x02,
#>     # 62: LISTSXP CAR (value)
#>     # 62-65: SYMSXP (symbol)
#>     0x00, 0x00, 0x00, 0x01,
#>     # 66-69: CHARSXP (string) | flags: ASCII
#>     0x00, 0x04, 0x00, 0x09,
#>     # 70-73: length of string in bytes: 4
#>     0x00, 0x00, 0x00, 0x04,
#>     #  b     a     s     e   
#>     0x62, 0x61, 0x73, 0x65,
#>     # 78: LISTSXP CDR (next node or NULL)
#>     # 78-81: LISTSXP (pairlist)
#>     0x00, 0x00, 0x00, 0x02,
#>     # 82: LISTSXP CAR (value)
#>     # 82-85: INTSXP (integer vector)
#>     0x00, 0x00, 0x00, 0x0d,
#>     # 86-89: length of vector: 1
#>     0x00, 0x00, 0x00, 0x01,
#>     c(
#>       # 90-93: 13L
#>       0x00, 0x00, 0x00, 0x0d
#>     ),
#>     # 94: LISTSXP CDR (next node or NULL)
#>     # 94-97: NILVALUE_SXP (NULL value)
#>     0x00, 0x00, 0x00, 0xfe,
#>     # 98: ALTREP data
#>     # 98-101: REALSXP (numeric vector)
#>     0x00, 0x00, 0x00, 0x0e,
#>     # 102-105: length of vector: 3
#>     0x00, 0x00, 0x00, 0x03,
#>     c(
#>       # 106-113: (-1)^0x00 * 2^(16 * 0x40 + 0x01 - 1023) * (1 + 0x4000000000000/2^52) == 5
#>       0x40, # binary: 0 1000000 => sign==0, exponent_upper==0x40
#>       0x14, # binary: 0001 0100 => exponent_lower==0x01, mantissa_upper==0x4
#>       0x00, 0x00, 0x00, 0x00, 0x00, 0x00 # mantissa_lower==0x000000000000
#>     ),
#>     c(
#>       # 114-121: (-1)^0x00 * 2^(16 * 0x3f + 0x0f - 1023) * (1 + 0x0000000000000/2^52) == 1
#>       0x3f, # binary: 0 0111111 => sign==0, exponent_upper==0x3f
#>       0xf0, # binary: 1111 0000 => exponent_lower==0x0f, mantissa_upper==0x0
#>       0x00, 0x00, 0x00, 0x00, 0x00, 0x00 # mantissa_lower==0x000000000000
#>     ),
#>     c(
#>       # 122-129: (-1)^0x00 * 2^(16 * 0x3f + 0x0f - 1023) * (1 + 0x0000000000000/2^52) == 1
#>       0x3f, # binary: 0 0111111 => sign==0, exponent_upper==0x3f
#>       0xf0, # binary: 1111 0000 => exponent_lower==0x0f, mantissa_upper==0x0
#>       0x00, 0x00, 0x00, 0x00, 0x00, 0x00 # mantissa_lower==0x000000000000
#>     ),
#>     # 130: ALTREP attributes (always present)
#>     # 130-133: NILVALUE_SXP (NULL value)
#>     0x00, 0x00, 0x00, 0xfe
#>   )
#> )))
construct_serialize(c("hello", "world"))
#> unserialize(as.raw(c(
#>   # --- HEADER ---
#>   c(
#>     # 1: serialization format, A for ASCII, X for binary, rawToChar(as.raw(0x58)) == 'X'
#>     0x58,
#>     # 2: always 0x0a, the bytecode for "\n", just a separator
#>     0x0a,
#>     # 3-6: The workspace format version, 3 since R 3.5.0, always 2 before that
#>     0x00, 0x00, 0x00, 0x03,
#>     # 7-10: The R version used to serialize, here 4.6.1
#>     0x00, 0x04, 0x06, 0x01,
#>     # 11-14: The minimal R version that can unserialize this, e.g. 3.5.0
#>     0x00, 0x03, 0x05, 0x00,
#>     # 15-18: The length of the character set name in bytes: 5
#>     0x00, 0x00, 0x00, 0x05,
#>     # 19-23: Which character set is used for the encoding, e.g. 'UTF-8'
#>     #  U     T     F     -     8
#>     0x55, 0x54, 0x46, 0x2d, 0x38
#>   ),
#>   # --- DATA ---
#>   c(
#>     # 24-27: STRSXP (character vector)
#>     0x00, 0x00, 0x00, 0x10,
#>     # 28-31: length of vector: 2
#>     0x00, 0x00, 0x00, 0x02,
#>     c(
#>       # 32-35: CHARSXP (string) | flags: ASCII
#>       0x00, 0x04, 0x00, 0x09,
#>       # 36-39: length of string in bytes: 5
#>       0x00, 0x00, 0x00, 0x05,
#>       #  h     e     l     l     o   
#>       0x68, 0x65, 0x6c, 0x6c, 0x6f
#>     ),
#>     c(
#>       # 45-48: CHARSXP (string) | flags: ASCII
#>       0x00, 0x04, 0x00, 0x09,
#>       # 49-52: length of string in bytes: 5
#>       0x00, 0x00, 0x00, 0x05,
#>       #  w     o     r     l     d   
#>       0x77, 0x6f, 0x72, 0x6c, 0x64
#>     )
#>   )
#> )))

# Complex objects
construct_serialize(data.frame(a = 1:3, b = letters[1:3]))
#> unserialize(as.raw(c(
#>   # --- HEADER ---
#>   c(
#>     # 1: serialization format, A for ASCII, X for binary, rawToChar(as.raw(0x58)) == 'X'
#>     0x58,
#>     # 2: always 0x0a, the bytecode for "\n", just a separator
#>     0x0a,
#>     # 3-6: The workspace format version, 3 since R 3.5.0, always 2 before that
#>     0x00, 0x00, 0x00, 0x03,
#>     # 7-10: The R version used to serialize, here 4.6.1
#>     0x00, 0x04, 0x06, 0x01,
#>     # 11-14: The minimal R version that can unserialize this, e.g. 3.5.0
#>     0x00, 0x03, 0x05, 0x00,
#>     # 15-18: The length of the character set name in bytes: 5
#>     0x00, 0x00, 0x00, 0x05,
#>     # 19-23: Which character set is used for the encoding, e.g. 'UTF-8'
#>     #  U     T     F     -     8
#>     0x55, 0x54, 0x46, 0x2d, 0x38
#>   ),
#>   # --- DATA ---
#>   c(
#>     # 24-27: VECSXP (list) | flags: IS_OBJECT, HAS_ATTR
#>     0x00, 0x00, 0x03, 0x13,
#>     # 28-31: length of list: 2
#>     0x00, 0x00, 0x00, 0x02,
#>     c(
#>       # 32-35: ALTREP_SXP (alternative representation)
#>       0x00, 0x00, 0x00, 0xee,
#>       # 36: ALTREP class info (pairlist)
#>       # 36-39: LISTSXP (pairlist)
#>       0x00, 0x00, 0x00, 0x02,
#>       # 40: LISTSXP CAR (value)
#>       # 40-43: SYMSXP (symbol)
#>       0x00, 0x00, 0x00, 0x01,
#>       # 44-47: CHARSXP (string) | flags: ASCII
#>       0x00, 0x04, 0x00, 0x09,
#>       # 48-51: length of string in bytes: 14
#>       0x00, 0x00, 0x00, 0x0e,
#>       #  c     o     m     p     a     c     t     _     i     n     t     s     
#>       0x63, 0x6f, 0x6d, 0x70, 0x61, 0x63, 0x74, 0x5f, 0x69, 0x6e, 0x74, 0x73,
#>       #  e     q   
#>       0x65, 0x71,
#>       # 66: LISTSXP CDR (next node or NULL)
#>       # 66-69: LISTSXP (pairlist)
#>       0x00, 0x00, 0x00, 0x02,
#>       # 70: LISTSXP CAR (value)
#>       # 70-73: SYMSXP (symbol)
#>       0x00, 0x00, 0x00, 0x01,
#>       # 74-77: CHARSXP (string) | flags: ASCII
#>       0x00, 0x04, 0x00, 0x09,
#>       # 78-81: length of string in bytes: 4
#>       0x00, 0x00, 0x00, 0x04,
#>       #  b     a     s     e   
#>       0x62, 0x61, 0x73, 0x65,
#>       # 86: LISTSXP CDR (next node or NULL)
#>       # 86-89: LISTSXP (pairlist)
#>       0x00, 0x00, 0x00, 0x02,
#>       # 90: LISTSXP CAR (value)
#>       # 90-93: INTSXP (integer vector)
#>       0x00, 0x00, 0x00, 0x0d,
#>       # 94-97: length of vector: 1
#>       0x00, 0x00, 0x00, 0x01,
#>       c(
#>         # 98-101: 13L
#>         0x00, 0x00, 0x00, 0x0d
#>       ),
#>       # 102: LISTSXP CDR (next node or NULL)
#>       # 102-105: NILVALUE_SXP (NULL value)
#>       0x00, 0x00, 0x00, 0xfe,
#>       # 106: ALTREP data
#>       # 106-109: REALSXP (numeric vector)
#>       0x00, 0x00, 0x00, 0x0e,
#>       # 110-113: length of vector: 3
#>       0x00, 0x00, 0x00, 0x03,
#>       c(
#>         # 114-121: (-1)^0x00 * 2^(16 * 0x40 + 0x00 - 1023) * (1 + 0x8000000000000/2^52) == 3
#>         0x40, # binary: 0 1000000 => sign==0, exponent_upper==0x40
#>         0x08, # binary: 0000 1000 => exponent_lower==0x00, mantissa_upper==0x8
#>         0x00, 0x00, 0x00, 0x00, 0x00, 0x00 # mantissa_lower==0x000000000000
#>       ),
#>       c(
#>         # 122-129: (-1)^0x00 * 2^(16 * 0x3f + 0x0f - 1023) * (1 + 0x0000000000000/2^52) == 1
#>         0x3f, # binary: 0 0111111 => sign==0, exponent_upper==0x3f
#>         0xf0, # binary: 1111 0000 => exponent_lower==0x0f, mantissa_upper==0x0
#>         0x00, 0x00, 0x00, 0x00, 0x00, 0x00 # mantissa_lower==0x000000000000
#>       ),
#>       c(
#>         # 130-137: (-1)^0x00 * 2^(16 * 0x3f + 0x0f - 1023) * (1 + 0x0000000000000/2^52) == 1
#>         0x3f, # binary: 0 0111111 => sign==0, exponent_upper==0x3f
#>         0xf0, # binary: 1111 0000 => exponent_lower==0x0f, mantissa_upper==0x0
#>         0x00, 0x00, 0x00, 0x00, 0x00, 0x00 # mantissa_lower==0x000000000000
#>       ),
#>       # 138: ALTREP attributes (always present)
#>       # 138-141: NILVALUE_SXP (NULL value)
#>       0x00, 0x00, 0x00, 0xfe
#>     ),
#>     c(
#>       # 142-145: STRSXP (character vector)
#>       0x00, 0x00, 0x00, 0x10,
#>       # 146-149: length of vector: 3
#>       0x00, 0x00, 0x00, 0x03,
#>       c(
#>         # 150-153: CHARSXP (string) | flags: ASCII
#>         0x00, 0x04, 0x00, 0x09,
#>         # 154-157: length of string in bytes: 1
#>         0x00, 0x00, 0x00, 0x01,
#>         #  a   
#>         0x61
#>       ),
#>       c(
#>         # 159-162: CHARSXP (string) | flags: ASCII
#>         0x00, 0x04, 0x00, 0x09,
#>         # 163-166: length of string in bytes: 1
#>         0x00, 0x00, 0x00, 0x01,
#>         #  b   
#>         0x62
#>       ),
#>       c(
#>         # 168-171: CHARSXP (string) | flags: ASCII
#>         0x00, 0x04, 0x00, 0x09,
#>         # 172-175: length of string in bytes: 1
#>         0x00, 0x00, 0x00, 0x01,
#>         #  c   
#>         0x63
#>       )
#>     ),
#>     # 177: Object attributes
#>     # 177-180: LISTSXP (pairlist) | flags: HAS_TAG
#>     0x00, 0x00, 0x04, 0x02,
#>     # 181: LISTSXP TAG (name)
#>     # 181-184: SYMSXP (symbol)
#>     0x00, 0x00, 0x00, 0x01,
#>     # 185-188: CHARSXP (string) | flags: ASCII
#>     0x00, 0x04, 0x00, 0x09,
#>     # 189-192: length of string in bytes: 5
#>     0x00, 0x00, 0x00, 0x05,
#>     #  n     a     m     e     s   
#>     0x6e, 0x61, 0x6d, 0x65, 0x73,
#>     # 198: LISTSXP CAR (value)
#>     # 198-201: STRSXP (character vector)
#>     0x00, 0x00, 0x00, 0x10,
#>     # 202-205: length of vector: 2
#>     0x00, 0x00, 0x00, 0x02,
#>     c(
#>       # 206-209: CHARSXP (string) | flags: ASCII
#>       0x00, 0x04, 0x00, 0x09,
#>       # 210-213: length of string in bytes: 1
#>       0x00, 0x00, 0x00, 0x01,
#>       #  a   
#>       0x61
#>     ),
#>     c(
#>       # 215-218: CHARSXP (string) | flags: ASCII
#>       0x00, 0x04, 0x00, 0x09,
#>       # 219-222: length of string in bytes: 1
#>       0x00, 0x00, 0x00, 0x01,
#>       #  b   
#>       0x62
#>     ),
#>     # 224: LISTSXP CDR (next node or NULL)
#>     # 224-227: LISTSXP (pairlist) | flags: HAS_TAG
#>     0x00, 0x00, 0x04, 0x02,
#>     # 228: LISTSXP TAG (name)
#>     # 228-231: SYMSXP (symbol)
#>     0x00, 0x00, 0x00, 0x01,
#>     # 232-235: CHARSXP (string) | flags: ASCII
#>     0x00, 0x04, 0x00, 0x09,
#>     # 236-239: length of string in bytes: 5
#>     0x00, 0x00, 0x00, 0x05,
#>     #  c     l     a     s     s   
#>     0x63, 0x6c, 0x61, 0x73, 0x73,
#>     # 245: LISTSXP CAR (value)
#>     # 245-248: STRSXP (character vector)
#>     0x00, 0x00, 0x00, 0x10,
#>     # 249-252: length of vector: 1
#>     0x00, 0x00, 0x00, 0x01,
#>     c(
#>       # 253-256: CHARSXP (string) | flags: ASCII
#>       0x00, 0x04, 0x00, 0x09,
#>       # 257-260: length of string in bytes: 10
#>       0x00, 0x00, 0x00, 0x0a,
#>       #  d     a     t     a     .     f     r     a     m     e   
#>       0x64, 0x61, 0x74, 0x61, 0x2e, 0x66, 0x72, 0x61, 0x6d, 0x65
#>     ),
#>     # 271: LISTSXP CDR (next node or NULL)
#>     # 271-274: LISTSXP (pairlist) | flags: HAS_TAG
#>     0x00, 0x00, 0x04, 0x02,
#>     # 275: LISTSXP TAG (name)
#>     # 275-278: SYMSXP (symbol)
#>     0x00, 0x00, 0x00, 0x01,
#>     # 279-282: CHARSXP (string) | flags: ASCII
#>     0x00, 0x04, 0x00, 0x09,
#>     # 283-286: length of string in bytes: 9
#>     0x00, 0x00, 0x00, 0x09,
#>     #  r     o     w     .     n     a     m     e     s   
#>     0x72, 0x6f, 0x77, 0x2e, 0x6e, 0x61, 0x6d, 0x65, 0x73,
#>     # 296: LISTSXP CAR (value)
#>     # 296-299: INTSXP (integer vector)
#>     0x00, 0x00, 0x00, 0x0d,
#>     # 300-303: length of vector: 2
#>     0x00, 0x00, 0x00, 0x02,
#>     c(
#>       # 304-307: NA_integer_
#>       0x80, 0x00, 0x00, 0x00
#>     ),
#>     c(
#>       # 308-311: -3L
#>       0xff, 0xff, 0xff, 0xfd
#>     ),
#>     # 312: LISTSXP CDR (next node or NULL)
#>     # 312-315: NILVALUE_SXP (NULL value)
#>     0x00, 0x00, 0x00, 0xfe
#>   )
#> )))
construct_serialize(matrix(1:6, nrow = 2))
#> unserialize(as.raw(c(
#>   # --- HEADER ---
#>   c(
#>     # 1: serialization format, A for ASCII, X for binary, rawToChar(as.raw(0x58)) == 'X'
#>     0x58,
#>     # 2: always 0x0a, the bytecode for "\n", just a separator
#>     0x0a,
#>     # 3-6: The workspace format version, 3 since R 3.5.0, always 2 before that
#>     0x00, 0x00, 0x00, 0x03,
#>     # 7-10: The R version used to serialize, here 4.6.1
#>     0x00, 0x04, 0x06, 0x01,
#>     # 11-14: The minimal R version that can unserialize this, e.g. 3.5.0
#>     0x00, 0x03, 0x05, 0x00,
#>     # 15-18: The length of the character set name in bytes: 5
#>     0x00, 0x00, 0x00, 0x05,
#>     # 19-23: Which character set is used for the encoding, e.g. 'UTF-8'
#>     #  U     T     F     -     8
#>     0x55, 0x54, 0x46, 0x2d, 0x38
#>   ),
#>   # --- DATA ---
#>   c(
#>     # 24-27: INTSXP (integer vector) | flags: HAS_ATTR
#>     0x00, 0x00, 0x02, 0x0d,
#>     # 28-31: length of vector: 6
#>     0x00, 0x00, 0x00, 0x06,
#>     c(
#>       # 32-35: 1L
#>       0x00, 0x00, 0x00, 0x01
#>     ),
#>     c(
#>       # 36-39: 2L
#>       0x00, 0x00, 0x00, 0x02
#>     ),
#>     c(
#>       # 40-43: 3L
#>       0x00, 0x00, 0x00, 0x03
#>     ),
#>     c(
#>       # 44-47: 4L
#>       0x00, 0x00, 0x00, 0x04
#>     ),
#>     c(
#>       # 48-51: 5L
#>       0x00, 0x00, 0x00, 0x05
#>     ),
#>     c(
#>       # 52-55: 6L
#>       0x00, 0x00, 0x00, 0x06
#>     ),
#>     # 56: Object attributes
#>     # 56-59: LISTSXP (pairlist) | flags: HAS_TAG
#>     0x00, 0x00, 0x04, 0x02,
#>     # 60: LISTSXP TAG (name)
#>     # 60-63: SYMSXP (symbol)
#>     0x00, 0x00, 0x00, 0x01,
#>     # 64-67: CHARSXP (string) | flags: ASCII
#>     0x00, 0x04, 0x00, 0x09,
#>     # 68-71: length of string in bytes: 3
#>     0x00, 0x00, 0x00, 0x03,
#>     #  d     i     m   
#>     0x64, 0x69, 0x6d,
#>     # 75: LISTSXP CAR (value)
#>     # 75-78: INTSXP (integer vector)
#>     0x00, 0x00, 0x00, 0x0d,
#>     # 79-82: length of vector: 2
#>     0x00, 0x00, 0x00, 0x02,
#>     c(
#>       # 83-86: 2L
#>       0x00, 0x00, 0x00, 0x02
#>     ),
#>     c(
#>       # 87-90: 3L
#>       0x00, 0x00, 0x00, 0x03
#>     ),
#>     # 91: LISTSXP CDR (next node or NULL)
#>     # 91-94: NILVALUE_SXP (NULL value)
#>     0x00, 0x00, 0x00, 0xfe
#>   )
#> )))

# Objects with attributes
x <- c(a = 1, b = 2, c = 3)
construct_serialize(x)
#> unserialize(as.raw(c(
#>   # --- HEADER ---
#>   c(
#>     # 1: serialization format, A for ASCII, X for binary, rawToChar(as.raw(0x58)) == 'X'
#>     0x58,
#>     # 2: always 0x0a, the bytecode for "\n", just a separator
#>     0x0a,
#>     # 3-6: The workspace format version, 3 since R 3.5.0, always 2 before that
#>     0x00, 0x00, 0x00, 0x03,
#>     # 7-10: The R version used to serialize, here 4.6.1
#>     0x00, 0x04, 0x06, 0x01,
#>     # 11-14: The minimal R version that can unserialize this, e.g. 3.5.0
#>     0x00, 0x03, 0x05, 0x00,
#>     # 15-18: The length of the character set name in bytes: 5
#>     0x00, 0x00, 0x00, 0x05,
#>     # 19-23: Which character set is used for the encoding, e.g. 'UTF-8'
#>     #  U     T     F     -     8
#>     0x55, 0x54, 0x46, 0x2d, 0x38
#>   ),
#>   # --- DATA ---
#>   c(
#>     # 24-27: REALSXP (numeric vector) | flags: HAS_ATTR
#>     0x00, 0x00, 0x02, 0x0e,
#>     # 28-31: length of vector: 3
#>     0x00, 0x00, 0x00, 0x03,
#>     c(
#>       # 32-39: (-1)^0x00 * 2^(16 * 0x3f + 0x0f - 1023) * (1 + 0x0000000000000/2^52) == 1
#>       0x3f, # binary: 0 0111111 => sign==0, exponent_upper==0x3f
#>       0xf0, # binary: 1111 0000 => exponent_lower==0x0f, mantissa_upper==0x0
#>       0x00, 0x00, 0x00, 0x00, 0x00, 0x00 # mantissa_lower==0x000000000000
#>     ),
#>     c(
#>       # 40-47: (-1)^0x00 * 2^(16 * 0x40 + 0x00 - 1023) * (1 + 0x0000000000000/2^52) == 2
#>       0x40, # binary: 0 1000000 => sign==0, exponent_upper==0x40
#>       0x00, # binary: 0000 0000 => exponent_lower==0x00, mantissa_upper==0x0
#>       0x00, 0x00, 0x00, 0x00, 0x00, 0x00 # mantissa_lower==0x000000000000
#>     ),
#>     c(
#>       # 48-55: (-1)^0x00 * 2^(16 * 0x40 + 0x00 - 1023) * (1 + 0x8000000000000/2^52) == 3
#>       0x40, # binary: 0 1000000 => sign==0, exponent_upper==0x40
#>       0x08, # binary: 0000 1000 => exponent_lower==0x00, mantissa_upper==0x8
#>       0x00, 0x00, 0x00, 0x00, 0x00, 0x00 # mantissa_lower==0x000000000000
#>     ),
#>     # 56: Object attributes
#>     # 56-59: LISTSXP (pairlist) | flags: HAS_TAG
#>     0x00, 0x00, 0x04, 0x02,
#>     # 60: LISTSXP TAG (name)
#>     # 60-63: SYMSXP (symbol)
#>     0x00, 0x00, 0x00, 0x01,
#>     # 64-67: CHARSXP (string) | flags: ASCII
#>     0x00, 0x04, 0x00, 0x09,
#>     # 68-71: length of string in bytes: 5
#>     0x00, 0x00, 0x00, 0x05,
#>     #  n     a     m     e     s   
#>     0x6e, 0x61, 0x6d, 0x65, 0x73,
#>     # 77: LISTSXP CAR (value)
#>     # 77-80: STRSXP (character vector)
#>     0x00, 0x00, 0x00, 0x10,
#>     # 81-84: length of vector: 3
#>     0x00, 0x00, 0x00, 0x03,
#>     c(
#>       # 85-88: CHARSXP (string) | flags: ASCII
#>       0x00, 0x04, 0x00, 0x09,
#>       # 89-92: length of string in bytes: 1
#>       0x00, 0x00, 0x00, 0x01,
#>       #  a   
#>       0x61
#>     ),
#>     c(
#>       # 94-97: CHARSXP (string) | flags: ASCII
#>       0x00, 0x04, 0x00, 0x09,
#>       # 98-101: length of string in bytes: 1
#>       0x00, 0x00, 0x00, 0x01,
#>       #  b   
#>       0x62
#>     ),
#>     c(
#>       # 103-106: CHARSXP (string) | flags: ASCII
#>       0x00, 0x04, 0x00, 0x09,
#>       # 107-110: length of string in bytes: 1
#>       0x00, 0x00, 0x00, 0x01,
#>       #  c   
#>       0x63
#>     ),
#>     # 112: LISTSXP CDR (next node or NULL)
#>     # 112-115: NILVALUE_SXP (NULL value)
#>     0x00, 0x00, 0x00, 0xfe
#>   )
#> )))

# Expressions and calls
construct_serialize(quote(mean(x)))
#> unserialize(as.raw(c(
#>   # --- HEADER ---
#>   c(
#>     # 1: serialization format, A for ASCII, X for binary, rawToChar(as.raw(0x58)) == 'X'
#>     0x58,
#>     # 2: always 0x0a, the bytecode for "\n", just a separator
#>     0x0a,
#>     # 3-6: The workspace format version, 3 since R 3.5.0, always 2 before that
#>     0x00, 0x00, 0x00, 0x03,
#>     # 7-10: The R version used to serialize, here 4.6.1
#>     0x00, 0x04, 0x06, 0x01,
#>     # 11-14: The minimal R version that can unserialize this, e.g. 3.5.0
#>     0x00, 0x03, 0x05, 0x00,
#>     # 15-18: The length of the character set name in bytes: 5
#>     0x00, 0x00, 0x00, 0x05,
#>     # 19-23: Which character set is used for the encoding, e.g. 'UTF-8'
#>     #  U     T     F     -     8
#>     0x55, 0x54, 0x46, 0x2d, 0x38
#>   ),
#>   # --- DATA ---
#>   c(
#>     # 24-27: LANGSXP (language/call)
#>     0x00, 0x00, 0x00, 0x06,
#>     # 28: LANGSXP CAR (function)
#>     # 28-31: SYMSXP (symbol)
#>     0x00, 0x00, 0x00, 0x01,
#>     # 32-35: CHARSXP (string) | flags: ASCII
#>     0x00, 0x04, 0x00, 0x09,
#>     # 36-39: length of string in bytes: 4
#>     0x00, 0x00, 0x00, 0x04,
#>     #  m     e     a     n   
#>     0x6d, 0x65, 0x61, 0x6e,
#>     # 44: LANGSXP CDR (arguments)
#>     # 44-47: LISTSXP (pairlist)
#>     0x00, 0x00, 0x00, 0x02,
#>     # 48: LISTSXP CAR (value)
#>     # 48-51: SYMSXP (symbol)
#>     0x00, 0x00, 0x00, 0x01,
#>     # 52-55: CHARSXP (string) | flags: ASCII
#>     0x00, 0x04, 0x00, 0x09,
#>     # 56-59: length of string in bytes: 1
#>     0x00, 0x00, 0x00, 0x01,
#>     #  x   
#>     0x78,
#>     # 61: LISTSXP CDR (next node or NULL)
#>     # 61-64: NILVALUE_SXP (NULL value)
#>     0x00, 0x00, 0x00, 0xfe
#>   )
#> )))
construct_serialize(expression(x + 1, y * 2))
#> unserialize(as.raw(c(
#>   # --- HEADER ---
#>   c(
#>     # 1: serialization format, A for ASCII, X for binary, rawToChar(as.raw(0x58)) == 'X'
#>     0x58,
#>     # 2: always 0x0a, the bytecode for "\n", just a separator
#>     0x0a,
#>     # 3-6: The workspace format version, 3 since R 3.5.0, always 2 before that
#>     0x00, 0x00, 0x00, 0x03,
#>     # 7-10: The R version used to serialize, here 4.6.1
#>     0x00, 0x04, 0x06, 0x01,
#>     # 11-14: The minimal R version that can unserialize this, e.g. 3.5.0
#>     0x00, 0x03, 0x05, 0x00,
#>     # 15-18: The length of the character set name in bytes: 5
#>     0x00, 0x00, 0x00, 0x05,
#>     # 19-23: Which character set is used for the encoding, e.g. 'UTF-8'
#>     #  U     T     F     -     8
#>     0x55, 0x54, 0x46, 0x2d, 0x38
#>   ),
#>   # --- DATA ---
#>   c(
#>     # 24-27: EXPRSXP (expression vector)
#>     0x00, 0x00, 0x00, 0x14,
#>     # 28-31: length of expression vector: 2
#>     0x00, 0x00, 0x00, 0x02,
#>     c(
#>       # 32-35: LANGSXP (language/call)
#>       0x00, 0x00, 0x00, 0x06,
#>       # 36: LANGSXP CAR (function)
#>       # 36-39: SYMSXP (symbol)
#>       0x00, 0x00, 0x00, 0x01,
#>       # 40-43: CHARSXP (string) | flags: ASCII
#>       0x00, 0x04, 0x00, 0x09,
#>       # 44-47: length of string in bytes: 1
#>       0x00, 0x00, 0x00, 0x01,
#>       #  +   
#>       0x2b,
#>       # 49: LANGSXP CDR (arguments)
#>       # 49-52: LISTSXP (pairlist)
#>       0x00, 0x00, 0x00, 0x02,
#>       # 53: LISTSXP CAR (value)
#>       # 53-56: SYMSXP (symbol)
#>       0x00, 0x00, 0x00, 0x01,
#>       # 57-60: CHARSXP (string) | flags: ASCII
#>       0x00, 0x04, 0x00, 0x09,
#>       # 61-64: length of string in bytes: 1
#>       0x00, 0x00, 0x00, 0x01,
#>       #  x   
#>       0x78,
#>       # 66: LISTSXP CDR (next node or NULL)
#>       # 66-69: LISTSXP (pairlist)
#>       0x00, 0x00, 0x00, 0x02,
#>       # 70: LISTSXP CAR (value)
#>       # 70-73: REALSXP (numeric vector)
#>       0x00, 0x00, 0x00, 0x0e,
#>       # 74-77: length of vector: 1
#>       0x00, 0x00, 0x00, 0x01,
#>       c(
#>         # 78-85: (-1)^0x00 * 2^(16 * 0x3f + 0x0f - 1023) * (1 + 0x0000000000000/2^52) == 1
#>         0x3f, # binary: 0 0111111 => sign==0, exponent_upper==0x3f
#>         0xf0, # binary: 1111 0000 => exponent_lower==0x0f, mantissa_upper==0x0
#>         0x00, 0x00, 0x00, 0x00, 0x00, 0x00 # mantissa_lower==0x000000000000
#>       ),
#>       # 86: LISTSXP CDR (next node or NULL)
#>       # 86-89: NILVALUE_SXP (NULL value)
#>       0x00, 0x00, 0x00, 0xfe
#>     ),
#>     c(
#>       # 90-93: LANGSXP (language/call)
#>       0x00, 0x00, 0x00, 0x06,
#>       # 94: LANGSXP CAR (function)
#>       # 94-97: SYMSXP (symbol)
#>       0x00, 0x00, 0x00, 0x01,
#>       # 98-101: CHARSXP (string) | flags: ASCII
#>       0x00, 0x04, 0x00, 0x09,
#>       # 102-105: length of string in bytes: 1
#>       0x00, 0x00, 0x00, 0x01,
#>       #  *   
#>       0x2a,
#>       # 107: LANGSXP CDR (arguments)
#>       # 107-110: LISTSXP (pairlist)
#>       0x00, 0x00, 0x00, 0x02,
#>       # 111: LISTSXP CAR (value)
#>       # 111-114: SYMSXP (symbol)
#>       0x00, 0x00, 0x00, 0x01,
#>       # 115-118: CHARSXP (string) | flags: ASCII
#>       0x00, 0x04, 0x00, 0x09,
#>       # 119-122: length of string in bytes: 1
#>       0x00, 0x00, 0x00, 0x01,
#>       #  y   
#>       0x79,
#>       # 124: LISTSXP CDR (next node or NULL)
#>       # 124-127: LISTSXP (pairlist)
#>       0x00, 0x00, 0x00, 0x02,
#>       # 128: LISTSXP CAR (value)
#>       # 128-131: REALSXP (numeric vector)
#>       0x00, 0x00, 0x00, 0x0e,
#>       # 132-135: length of vector: 1
#>       0x00, 0x00, 0x00, 0x01,
#>       c(
#>         # 136-143: (-1)^0x00 * 2^(16 * 0x40 + 0x00 - 1023) * (1 + 0x0000000000000/2^52) == 2
#>         0x40, # binary: 0 1000000 => sign==0, exponent_upper==0x40
#>         0x00, # binary: 0000 0000 => exponent_lower==0x00, mantissa_upper==0x0
#>         0x00, 0x00, 0x00, 0x00, 0x00, 0x00 # mantissa_lower==0x000000000000
#>       ),
#>       # 144: LISTSXP CDR (next node or NULL)
#>       # 144-147: NILVALUE_SXP (NULL value)
#>       0x00, 0x00, 0x00, 0xfe
#>     )
#>   )
#> )))

# Special values
construct_serialize(c(NA, NaN, Inf, -Inf))
#> unserialize(as.raw(c(
#>   # --- HEADER ---
#>   c(
#>     # 1: serialization format, A for ASCII, X for binary, rawToChar(as.raw(0x58)) == 'X'
#>     0x58,
#>     # 2: always 0x0a, the bytecode for "\n", just a separator
#>     0x0a,
#>     # 3-6: The workspace format version, 3 since R 3.5.0, always 2 before that
#>     0x00, 0x00, 0x00, 0x03,
#>     # 7-10: The R version used to serialize, here 4.6.1
#>     0x00, 0x04, 0x06, 0x01,
#>     # 11-14: The minimal R version that can unserialize this, e.g. 3.5.0
#>     0x00, 0x03, 0x05, 0x00,
#>     # 15-18: The length of the character set name in bytes: 5
#>     0x00, 0x00, 0x00, 0x05,
#>     # 19-23: Which character set is used for the encoding, e.g. 'UTF-8'
#>     #  U     T     F     -     8
#>     0x55, 0x54, 0x46, 0x2d, 0x38
#>   ),
#>   # --- DATA ---
#>   c(
#>     # 24-27: REALSXP (numeric vector)
#>     0x00, 0x00, 0x00, 0x0e,
#>     # 28-31: length of vector: 4
#>     0x00, 0x00, 0x00, 0x04,
#>     c(
#>       # 32-39: NA_real_
#>       0x7f, 0xf0, 0x00, 0x00, 0x00, 0x00, 0x07, 0xa2
#>     ),
#>     c(
#>       # 40-47: NaN (standard pattern)
#>       0x7f, 0xf8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
#>     ),
#>     c(
#>       # 48-55: Inf
#>       0x7f, 0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
#>     ),
#>     c(
#>       # 56-63: -Inf
#>       0xff, 0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
#>     )
#>   )
#> )))

# Builtin functions
construct_serialize(sum)
#> unserialize(as.raw(c(
#>   # --- HEADER ---
#>   c(
#>     # 1: serialization format, A for ASCII, X for binary, rawToChar(as.raw(0x58)) == 'X'
#>     0x58,
#>     # 2: always 0x0a, the bytecode for "\n", just a separator
#>     0x0a,
#>     # 3-6: The workspace format version, 3 since R 3.5.0, always 2 before that
#>     0x00, 0x00, 0x00, 0x03,
#>     # 7-10: The R version used to serialize, here 4.6.1
#>     0x00, 0x04, 0x06, 0x01,
#>     # 11-14: The minimal R version that can unserialize this, e.g. 3.5.0
#>     0x00, 0x03, 0x05, 0x00,
#>     # 15-18: The length of the character set name in bytes: 5
#>     0x00, 0x00, 0x00, 0x05,
#>     # 19-23: Which character set is used for the encoding, e.g. 'UTF-8'
#>     #  U     T     F     -     8
#>     0x55, 0x54, 0x46, 0x2d, 0x38
#>   ),
#>   # --- DATA ---
#>   c(
#>     # 24-27: BUILTINSXP (builtin function)
#>     0x00, 0x00, 0x00, 0x08,
#>     # 28-31: BUILTINSXP name length: 3
#>     0x00, 0x00, 0x00, 0x03,
#>     # 32-34: BUILTINSXP name: "sum"
#>     0x73, 0x75, 0x6d
#>   )
#> )))
construct_serialize(`if`)
#> unserialize(as.raw(c(
#>   # --- HEADER ---
#>   c(
#>     # 1: serialization format, A for ASCII, X for binary, rawToChar(as.raw(0x58)) == 'X'
#>     0x58,
#>     # 2: always 0x0a, the bytecode for "\n", just a separator
#>     0x0a,
#>     # 3-6: The workspace format version, 3 since R 3.5.0, always 2 before that
#>     0x00, 0x00, 0x00, 0x03,
#>     # 7-10: The R version used to serialize, here 4.6.1
#>     0x00, 0x04, 0x06, 0x01,
#>     # 11-14: The minimal R version that can unserialize this, e.g. 3.5.0
#>     0x00, 0x03, 0x05, 0x00,
#>     # 15-18: The length of the character set name in bytes: 5
#>     0x00, 0x00, 0x00, 0x05,
#>     # 19-23: Which character set is used for the encoding, e.g. 'UTF-8'
#>     #  U     T     F     -     8
#>     0x55, 0x54, 0x46, 0x2d, 0x38
#>   ),
#>   # --- DATA ---
#>   c(
#>     # 24-27: SPECIALSXP (special function)
#>     0x00, 0x00, 0x00, 0x07,
#>     # 28-31: SPECIALSXP name length: 2
#>     0x00, 0x00, 0x00, 0x02,
#>     # 32-33: SPECIALSXP name: "if"
#>     0x69, 0x66
#>   )
#> )))

# The generated code can be evaluated to reconstruct the object
code <- construct_serialize(iris)
reconstructed <- eval(parse(text = paste(code, collapse = "\n")))
identical(reconstructed, iris)
#> [1] TRUE
```
