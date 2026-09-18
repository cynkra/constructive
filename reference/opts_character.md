# Constructive options for type 'character'

These options will be used on objects of type 'character'. This type has
a single native constructor, but some additional options can be set.

`unicode_representation` and `escape` are usually better set in the main
function
([`construct()`](https://cynkra.github.io/constructive/reference/construct.md)
or other) so they apply not only on strings but on symbols and argument
names as well.

To set options on all atomic types at once see
[opts_atomic](https://cynkra.github.io/constructive/reference/opts_atomic.md)().

## Usage

``` r
opts_character(
  constructor = c("default"),
  ...,
  trim = NULL,
  fill = c("default", "rlang", "+", "...", "none"),
  compress = TRUE,
  unicode_representation = c("ascii", "latin", "character", "unicode"),
  escape = FALSE
)
```

## Arguments

- constructor:

  String. Method used to construct the object, often the name of a
  function.

- ...:

  Constructive options built with the `opts_*()` family of functions.
  See the "Constructive options" section below.

- trim:

  `NULL` or integerish. Maximum of elements showed before it's trimmed.
  Note that it will necessarily produce code that doesn't reproduce the
  input. This code will parse without failure but its evaluation might
  fail.

- fill:

  String. Method to use to represent the trimmed elements. See
  [`?opts_atomic`](https://cynkra.github.io/constructive/reference/opts_atomic.md)

- compress:

  Boolean. If `TRUE` instead of [`c()`](https://rdrr.io/r/base/c.html)
  Use [`seq()`](https://rdrr.io/r/base/seq.html),
  [`rep()`](https://rdrr.io/r/base/rep.html) when relevant to simplify
  the output.

- unicode_representation:

  By default "ascii", which means only ASCII characters (code point
  \< 128) will be used to construct strings and variable names. This
  makes sure that homoglyphs (different spaces and other identically
  displayed unicode characters) are printed differently, and avoid
  possible unfortunate copy and paste auto conversion issues. "latin" is
  more lax and uses all latin characters (code point \< 256).
  "character" shows all characters, but not emojis. Finally "unicode"
  displays all characters and emojis, which is what
  [`dput()`](https://rdrr.io/r/base/dput.html) does.

- escape:

  Boolean. Whether to escape double quotes and backslashes. If `FALSE`
  we use single quotes to surround strings (including variable and
  element names) containing double quotes, and raw strings for strings
  that contain backslashes and/or a combination of single and double
  quotes. Depending on `unicode_representation` `escape = FALSE` cannot
  be applied on all strings.

## Value

An object of class
\<constructive_options/constructive_options_character\>
