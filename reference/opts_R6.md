# Constructive options for class 'R6'

These options will be used on objects of class 'R6'.

## Usage

``` r
opts_R6(constructor = c("R6Class", "next"), ...)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class \<constructive_options/constructive_options_R6\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"R6Class"` (default): We build the object using `R6Class()$new()`,
  see details.

- `"next"` : Use the constructor for the next supported class.

Objects of class "R6" are harder to construct than "R6ClassGenerator"
objects, because 'constructive' doesn't know by default the constructor
(i.e. class generator) that was used to build them. So what we do is we
build a class generator that generates this object by default. This is
why the generated code is in the form `R6Class()$new()`.

Another layer of complexity is added when the object features an
`initialize()` method, we cannot implement it in the class generator
because it might change the behavior of `$new()` and return a wrong
result (or fail). For this reason the `initialize()` method when it
exists is repaired as an extra step.

[`construct_diff()`](https://cynkra.github.io/constructive/reference/construct_diff.md)
works well to inspect the differences between two R6 objects where
alternatives like
[`waldo::compare()`](https://waldo.r-lib.org/reference/compare.html) or
[`base::all.equal()`](https://rdrr.io/r/base/all.equal.html) don't
return anything informative.
