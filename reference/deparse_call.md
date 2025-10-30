# Deparse a language object

An alternative to
[`base::deparse()`](https://rdrr.io/r/base/deparse.html) and
[`rlang::expr_deparse()`](https://rlang.r-lib.org/reference/expr_print.html)
that handles additional corner cases and fails when encountering tokens
other than symbols and syntactic literals where cited alternatives would
produce non syntactic code.  
  

## Usage

``` r
deparse_call(
  call,
  one_liner = FALSE,
  pipe = FALSE,
  style = TRUE,
  collapse = !style,
  unicode_representation = c("ascii", "latin", "character", "unicode"),
  escape = FALSE,
  pedantic_encoding = FALSE
)
```

## Arguments

- call:

  A call.

- one_liner:

  Boolean. Whether to collapse multi-line expressions on a single line
  using semicolons.

- pipe:

  Boolean. Whether to use the base pipe to disentangle nested calls.
  This works best on simple calls.

- style:

  Boolean. Whether to give a class "constructive_code" on the output for
  pretty printing.

- collapse:

  Boolean. Whether to collapse the output to a single string, won't be
  directly visible if `style` is `TRUE`.

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

## Value

a string or a character vector, with a class "constructive_code" for
pretty printing if `style` is `TRUE`.

## Examples

``` r
expr <- quote(foo(bar({this; that}, 1)))
deparse_call(expr)
#> foo(bar({
#>       this
#>       that
#>     }, 1))
deparse_call(expr, one_liner = TRUE)
#> foo(bar({this; that}, 1))
deparse_call(expr, pipe = TRUE)
#> {
#>   this
#>   that
#> } |> bar(1) |> foo()
deparse_call(expr, style = FALSE)
#> [1] "foo(bar({\n      this\n      that\n    }, 1))"
```
