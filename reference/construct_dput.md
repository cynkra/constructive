# Construct using only low level constructors

- `construct_dput()` is a closer counterpart to
  [`base::dput()`](https://rdrr.io/r/base/dput.html) that doesn't use
  higher level constructors such as
  [`data.frame()`](https://rdrr.io/r/base/data.frame.html) and
  [`factor()`](https://rdrr.io/r/base/factor.html).

- `construct_base()` uses higher constructors, but only for the classes
  maintained in the default base R packages. This includes
  [`data.frame()`](https://rdrr.io/r/base/data.frame.html) and
  [`factor()`](https://rdrr.io/r/base/factor.html), the S4 constructors
  from the 'method' package etc, but not `data.table()` and other
  constructors for classes from other packages.

## Usage

``` r
construct_dput(
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
  template = getOption("constructive_opts_template")
)

construct_base(
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
  template = getOption("constructive_opts_template")
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

## Value

An object of class 'constructive'.

## Details

Both functions are valuable for object inspection, and might provide
more stable snapshots, since supporting more classes in the package
means the default output of
[`construct()`](https://cynkra.github.io/constructive/reference/construct.md)
might change over time for some objects.

To use higher level constructor from the base package itself, excluding
for instance [`stats::ts()`](https://rdrr.io/r/stats/ts.html),
[`utils::person()`](https://rdrr.io/r/utils/person.html) or
`methods::classGeneratorFunction()`), we can call
`construct(x, classes = "{base}"`

## Examples

``` r
construct_dput(head(iris, 2))
#> list(
#>   Sepal.Length = c(5.1, 4.9),
#>   Sepal.Width = c(3.5, 3),
#>   Petal.Length = c(1.4, 1.4),
#>   Petal.Width = c(0.2, 0.2),
#>   Species = c(1L, 1L) |>
#>     structure(levels = c("setosa", "versicolor", "virginica"), class = "factor")
#> ) |>
#>   structure(row.names = c(NA, -2L), class = "data.frame")
construct_base(head(iris, 2))
#> data.frame(
#>   Sepal.Length = c(5.1, 4.9),
#>   Sepal.Width = c(3.5, 3),
#>   Petal.Length = 1.4,
#>   Petal.Width = 0.2,
#>   Species = factor("setosa", levels = c("setosa", "versicolor", "virginica"))
#> )
```
