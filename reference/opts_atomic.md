# Constructive options for atomic types

These options will be used on atomic types ("logical", "integer",
"numeric", "complex", "character" and "raw"). They can also be directly
provided to atomic types through their own `opts_*()` function, and in
this case the latter will have precedence.

## Usage

``` r
opts_atomic(
  ...,
  trim = NULL,
  fill = c("default", "rlang", "+", "...", "none"),
  compress = TRUE
)
```

## Arguments

- ...:

  Additional options used by user defined constructors through the
  `opts` object

- trim:

  `NULL` or integerish. Maximum of elements showed before it's trimmed.
  Note that it will necessarily produce code that doesn't reproduce the
  input. This code will parse without failure but its evaluation might
  fail.

- fill:

  String. Method to use to represent the trimmed elements.

- compress:

  Boolean. If `TRUE` instead of [`c()`](https://rdrr.io/r/base/c.html)
  Use [`seq()`](https://rdrr.io/r/base/seq.html),
  [`rep()`](https://rdrr.io/r/base/rep.html) when relevant to simplify
  the output.

## Value

An object of class \<constructive_options/constructive_options_atomic\>

## Details

If `trim` is provided, depending on `fill` we will present trimmed
elements as followed:

- `"default"` : Use default atomic constructors, so for instance
  `c("a", "b", "c")` might become `c("a", character(2))`.

- `"rlang"` : Use rlang atomic constructors, so for instance
  `c("a", "b", "c")` might become `c("a", rlang::new_character(2))`,
  these `rlang` constructors create vectors of `NAs`, so it's different
  from the default option.

- `"+"`: Use unary `+`, so for instance `c("a", "b", "c")` might become
  `c("a", +2)`.

- `"..."`: Use `...`, so for instance `c("a", "b", "c")` might become
  `c("a", ...)`

- `"none"`: Don't represent trimmed elements.

Depending on the case some or all of the choices above might generate
code that cannot be executed. The 2 former options above are the most
likely to succeed and produce an output of the same type and dimensions
recursively. This would at least be the case for data frame.

## Examples

``` r
construct(iris, opts_atomic(trim = 2), check = FALSE) # fill = "default"
#> data.frame(
#>   Sepal.Length = c(5.1, 4.9, numeric(148)),
#>   Sepal.Width = c(3.5, 3, numeric(148)),
#>   Petal.Length = c(1.4, 1.4, numeric(148)),
#>   Petal.Width = c(0.2, 0.2, numeric(148)),
#>   Species = factor(c("setosa", "setosa", character(148)))
#> )
construct(iris, opts_atomic(trim = 2, fill = "rlang"), check = FALSE)
#> data.frame(
#>   Sepal.Length = c(5.1, 4.9, rlang::new_double(148)),
#>   Sepal.Width = c(3.5, 3, rlang::new_double(148)),
#>   Petal.Length = c(1.4, 1.4, rlang::new_double(148)),
#>   Petal.Width = c(0.2, 0.2, rlang::new_double(148)),
#>   Species = factor(c("setosa", "setosa", rlang::new_character(148)))
#> )
construct(iris, opts_atomic(trim = 2, fill = "+"), check = FALSE)
#> data.frame(
#>   Sepal.Length = c(5.1, 4.9, +148),
#>   Sepal.Width = c(3.5, 3, +148),
#>   Petal.Length = c(1.4, 1.4, +148),
#>   Petal.Width = c(0.2, 0.2, +148),
#>   Species = factor(c("setosa", "setosa", +148))
#> )
construct(iris, opts_atomic(trim = 2, fill = "..."), check = FALSE)
#> data.frame(
#>   Sepal.Length = c(5.1, 4.9, ...),
#>   Sepal.Width = c(3.5, 3, ...),
#>   Petal.Length = c(1.4, 1.4, ...),
#>   Petal.Width = c(0.2, 0.2, ...),
#>   Species = factor(c("setosa", "setosa", ...))
#> )
construct(iris, opts_atomic(trim = 2, fill = "none"), check = FALSE)
#> data.frame(
#>   Sepal.Length = c(5.1, 4.9),
#>   Sepal.Width = c(3.5, 3),
#>   Petal.Length = c(1.4, 1.4),
#>   Petal.Width = c(0.2, 0.2),
#>   Species = factor(c("setosa", "setosa"))
#> )
construct(iris, opts_atomic(trim = 2, fill = "none"), check = FALSE)
#> data.frame(
#>   Sepal.Length = c(5.1, 4.9),
#>   Sepal.Width = c(3.5, 3),
#>   Petal.Length = c(1.4, 1.4),
#>   Petal.Width = c(0.2, 0.2),
#>   Species = factor(c("setosa", "setosa"))
#> )
x <- c("a a", "a\U000000A0a", "a\U00002002a", "\U430 \U430")
```
