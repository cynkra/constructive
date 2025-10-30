# .cstr_apply

Exported for custom constructor design. If `recurse` is `TRUE`
(default), we recurse to construct `args` and insert their construction
code in a `fun(...)` call returned as a character vector. If `args`
already contains code rather than object to construct one should set
`recurse` to `FALSE`.

## Usage

``` r
.cstr_apply(
  args,
  fun = "list",
  ...,
  trailing_comma = FALSE,
  recurse = TRUE,
  implicit_names = FALSE,
  new_line = TRUE,
  one_liner = FALSE,
  unicode_representation = c("ascii", "latin", "character", "unicode"),
  escape = FALSE
)
```

## Arguments

- args:

  A list of arguments to construct recursively, or code if
  `recurse = FALSE`. If elements are named, the arguments will be named
  in the generated code.

- fun:

  The function name to use to build code of the form "fun(...)"

- ...:

  Options passed recursively to the further methods

- trailing_comma:

  Boolean. Whether to leave a trailing comma after the last argument if
  the code is multiline, some constructors allow it (e.g.
  [`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html))
  and it makes for nicer diffs in version control.

- recurse:

  Boolean. Whether to recursively generate the code to construct `args`.
  If `FALSE` arguments are expected to contain code.

- implicit_names:

  When data is provided, compress calls of the form `f(a = a)` to `f(a)`

- new_line:

  Boolean. Forwarded to `wrap()` to add a line between "fun(" and ")",
  forced to `FALSE` if `one_liner` is `TRUE`

- one_liner:

  Boolean. Whether to return a one line call.

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

A character vector of code

## Examples

``` r
a <- 1
.cstr_apply(list(a=a), "foo")
#> [1] "foo(a = 1)"
.cstr_apply(list(a=a), "foo", data = list(a=1))
#> [1] "foo(a = a)"
.cstr_apply(list(a=a), "foo", data = list(a=1), implicit_names = TRUE)
#> [1] "foo(a)"
.cstr_apply(list(b=a), "foo", data = list(a=1), implicit_names = TRUE)
#> [1] "foo(b = a)"
.cstr_apply(list(a="c(1,2)"), "foo")
#> [1] "foo(a = \"c(1,2)\")"
.cstr_apply(list(a="c(1,2)"), "foo", recurse = FALSE)
#> [1] "foo(a = c(1,2))"
```
