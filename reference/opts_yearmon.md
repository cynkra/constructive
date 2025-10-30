# Constructive options for class 'yearmon'

These options will be used on objects of class 'yearmon'.

## Usage

``` r
opts_yearmon(constructor = c("as.yearmon", "yearmon", "next"), ...)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class \<constructive_options/constructive_options_yearmon\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"as.yearmon"` (default): We build the object using
  [`zoo::as.yearmon()`](https://rdrr.io/pkg/zoo/man/yearmon.html) on a
  string in the format `"2000 Q3"`.

- `"yearmon"` : We build the object using
  [`zoo::yearmon()`](https://rdrr.io/pkg/zoo/man/yearmon.html) on a
  double in the format `2000.5`

- `"next"` : Use the constructor for the next supported class.
