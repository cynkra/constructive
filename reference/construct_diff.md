# Display diff of object definitions

This calls
[`construct()`](https://cynkra.github.io/constructive/reference/construct.md)
on two objects and compares the output using
[`diffobj::diffChr()`](https://rdrr.io/pkg/diffobj/man/diffChr.html).

## Usage

``` r
construct_diff(
  target,
  current,
  ...,
  data = NULL,
  pipe = NULL,
  check = TRUE,
  compare = compare_options(),
  one_liner = FALSE,
  template = getOption("constructive_opts_template"),
  classes = NULL,
  mode = c("sidebyside", "auto", "unified", "context"),
  interactive = TRUE
)
```

## Arguments

- target:

  the reference object

- current:

  the object being compared to `target`

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

- mode, interactive:

  passed to
  [`diffobj::diffChr()`](https://rdrr.io/pkg/diffobj/man/diffChr.html)

## Value

Returns `NULL` invisibly, called for side effects

## Examples

``` r
if (FALSE) { # \dontrun{
# some object print the same though they're different
# `construct_diff()` shows how they differ :
df1 <- data.frame(a=1, b = "x")
df2 <- data.frame(a=1L, b = "x", stringsAsFactors = TRUE)
attr(df2, "some_attribute") <- "a value"
df1
df2
construct_diff(df1, df2)


# Those are made easy to compare
construct_diff(substr, substring)
construct_diff(month.abb, month.name)

# more examples borrowed from {waldo} package
construct_diff(c("a", "b", "c"), c("a", "B", "c"))
construct_diff(c("X", letters), c(letters, "X"))
construct_diff(list(factor("x")), list(1L))
construct_diff(df1, df2)
x <- list(a = list(b = list(c = list(structure(1, e = 1)))))
y <- list(a = list(b = list(c = list(structure(1, e = "a")))))
construct_diff(x, y)
} # }
```
