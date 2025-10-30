# Construct a function's signature

Construct a function's signature such as the one you can see right below
in the 'Usage' section.

## Usage

``` r
construct_signature(x, name = NULL, one_liner = FALSE, style = TRUE)
```

## Arguments

- x:

  A function

- name:

  The name of the function, by default we use the symbol provided to `x`

- one_liner:

  Boolean. Whether to collapse multi-line expressions on a single line
  using semicolons.

- style:

  Boolean. Whether to give a class "constructive_code" on the output for
  pretty printing.

## Value

a string or a character vector, with a class "constructive_code" for
pretty printing if `style` is `TRUE`

## Examples

``` r
construct_signature(lm)
#> lm(formula, data, subset, weights, na.action, method = "qr", model = TRUE, x = FALSE, y = FALSE, qr = TRUE, singular.ok = TRUE, contrasts = NULL, offset, ...)
```
