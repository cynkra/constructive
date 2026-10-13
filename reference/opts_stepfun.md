# Constructive options for class 'stepfun'

These options will be used on objects of class 'stepfun'.

## Usage

``` r
opts_stepfun(constructor = c("stepfun", "next"), ...)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object, see Details
  section.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class \<constructive_options/constructive_options_stepfun\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"stepfun"` (default): We build the object using
  [`stepfun()`](https://rdrr.io/r/stats/stepfun.html), the arguments are
  retrieved from the function's enclosure. The "call" attribute is
  repaired if it differs from the constructed call.

- `"next"` : Use the constructor for the next supported class. Call
  [`.class2()`](https://rdrr.io/r/base/class.html) on the object to see
  in which order the methods will be tried. This will usually be
  equivalent to
  [`opts_function()`](https://cynkra.github.io/constructive/reference/opts_function.md).
