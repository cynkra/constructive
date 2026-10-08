# Constructive options for class 'Surv'

These options will be used on objects of class 'Surv'.

## Usage

``` r
opts_Surv(constructor = c("Surv", "next"), ...)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class \<constructive_options/constructive_options_Surv\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"Surv"` (default): We build the object using
  [`survival::Surv()`](https://rdrr.io/pkg/survival/man/Surv.html) on
  the columns of the object. Interval censored data is built using
  `type = "interval2"` whenever possible, `type = "interval"` otherwise.
  Multi-state data is built by providing a factor to the `event`
  argument.

- `"next"` : Use the constructor for the next supported class.
