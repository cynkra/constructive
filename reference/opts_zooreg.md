# Constructive options for class 'zooreg'

These options will be used on objects of class 'zooreg'.

## Usage

``` r
opts_zooreg(constructor = c("zooreg", "next"), ...)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class \<constructive_options/constructive_options_zooreg\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"zooreg"` (default): We build the object using
  [`zoo::zooreg()`](https://rdrr.io/pkg/zoo/man/zooreg.html), using the
  `start` and `frequency` arguments.

- `"next"` : Use the constructor for the next supported class.
