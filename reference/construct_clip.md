# Construct to clipboard

This is a simple wrapper for convenience, `construct_clip(x, ...)` is
equivalent to `print(construct(x, ...), print_mode = "clipboard")` (an
idiom that you might use to use the clipboard with other functions). For
more flexible printing options see
[`?constructive_print_mode`](https://cynkra.github.io/constructive/reference/constructive-global_options.md).

## Usage

``` r
construct_clip(
  x,
  ...,
  data = NULL,
  pipe = NULL,
  check = NULL,
  unicode_representation = c("ascii", "latin", "character", "unicode"),
  escape = FALSE,
  pedantic_encoding = FALSE,
  compare = compare_options(),
  one_liner = FALSE,
  template = getOption("constructive_opts_template"),
  classes = NULL
)
```

## Arguments

- x:

  An object, for
  [`construct_multi()`](https://cynkra.github.io/constructive/reference/construct.md)
  a named list or an environment.

- ...:

  Constructive options built with the `opts_*()` family of functions.
  See the "Constructive options" section below.

- data:

  Named list or environment of objects we want to detect and mention by
  name (as opposed to deparsing them further). Can also contain unnamed
  nested lists, environments, or package names, in the latter case
  package exports and datasets will be considered. In case of conflict,
  the last provided name is considered.

- pipe:

  Which pipe to use, either `"base"` or `"magrittr"`. Defaults to
  `"base"` for R \>= 4.2, otherwise to `"magrittr"`.

- check:

  Boolean. Whether to check if the created code reproduces the object
  using
  [`waldo::compare()`](https://waldo.r-lib.org/reference/compare.html).

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

- pedantic_encoding:

  Boolean. Whether to mark strings with the "unknown" encoding rather
  than an explicit native encoding ("UTF-8" or "latin1") when it's
  necessary to reproduce the binary representation exactly. This detail
  is normally of very little significance. The reason why we're not
  pedantic by default is that the constructed code might be different in
  the console and in snapshot tests and reprexes due to the latter
  rounding some angles, and it would be confusing for users.

- compare:

  Parameters passed to
  [`waldo::compare()`](https://waldo.r-lib.org/reference/compare.html),
  built with
  [`compare_options()`](https://cynkra.github.io/constructive/reference/compare_options.md).

- one_liner:

  Boolean. Whether to collapse the output to a single line of code.

- template:

  A list of constructive options built with `opts_*()` functions, they
  will be overriden by `...`. Use it to set a default behavior for
  `{constructive}`.

- classes:

  A character vector of classes for which to use idiomatic constructors
  when available, we can provide a package instead of all its classes,
  in the "{pkg}" form, and we can use a minus sign (inside the quotes)
  to exclude rather than include. By default we use idiomatic
  constructors whenever possible. The special values `"*none*"` and
  `"*base*"` can be used to restrict the idiomatic construction to the
  objects. See
  [`construct_dput()`](https://cynkra.github.io/constructive/reference/construct_dput.md)
  and
  [`construct_base()`](https://cynkra.github.io/constructive/reference/construct_dput.md)
  for wrappers around this feature.

## Value

An object of class 'constructive', invisibly. Called for side effects.

## Examples

``` r
if (FALSE) { # \dontrun{
construct_clip(head(cars))
} # }
```
