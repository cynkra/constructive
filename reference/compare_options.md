# Options for waldo::compare

Builds options that will be passed to
[`waldo::compare()`](https://waldo.r-lib.org/reference/compare.html)
down the line.

## Usage

``` r
compare_options(
  ignore_srcref = TRUE,
  ignore_attr = FALSE,
  ignore_function_env = FALSE,
  ignore_formula_env = FALSE
)
```

## Arguments

- ignore_srcref:

  Ignore differences in function `srcref`s? `TRUE` by default since the
  `srcref` does not change the behaviour of a function, only its printed
  representation.

- ignore_attr:

  Ignore differences in specified attributes? Supply a character vector
  to ignore differences in named attributes. By default the
  `"waldo_opts"` attribute is listed in `ignore_attr` so that changes to
  it are not reported; if you customize `ignore_attr`, you will probably
  want to do this yourself.

  For backward compatibility with
  [`all.equal()`](https://rdrr.io/r/base/all.equal.html), you can also
  use `TRUE`, to all ignore differences in all attributes. This is not
  generally recommended as it is a blunt tool that will ignore many
  important functional differences.

- ignore_function_env, ignore_formula_env:

  Ignore the environments of functions and formulas, respectively? These
  are provided primarily for backward compatibility with
  [`all.equal()`](https://rdrr.io/r/base/all.equal.html) which always
  ignores these environments.

## Value

A list
