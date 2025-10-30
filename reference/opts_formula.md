# Constructive options for formulas

These options will be used on formulas, defined as calls to `~`,
regardless of their `"class"` attribute.

## Usage

``` r
opts_formula(
  constructor = c("default", "formula", "as.formula", "new_formula", "next"),
  ...,
  environment = TRUE
)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object, see Details
  section.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

- environment:

  Boolean. Whether to attempt to construct the environment, if it makes
  a difference to construct it.

  Depending on `constructor`, we construct the formula as follows:

  - `"default"`: We construct the formula in the most common way using
    the `~` operator.

  - `"formula"` : deparse the formula as a string and use
    `base::formula()` on top of it.

  - `"as.formula"` : Same as above, but using `base::as.formula()`.

  - `"new_formula"` : extract both sides of the formula as separate
    language objects and feed them to
    [`rlang::new_formula()`](https://rlang.r-lib.org/reference/new_formula.html),
    along with the reconstructed environment if relevant.

## Value

An object of class \<constructive_options/constructive_options_formula\>
