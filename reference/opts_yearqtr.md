# Constructive options for class 'yearqtr'

These options will be used on objects of class 'yearqtr'.

## Usage

``` r
opts_yearqtr(constructor = c("as.yearqtr", "yearqtr", "next"), ...)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class \<constructive_options/constructive_options_yearqtr\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"as.yearqtr"` (default): We build the object using
  [`zoo::as.yearqtr()`](https://rdrr.io/pkg/zoo/man/yearqtr.html) on a
  string in the format `"2000 Q3"`.

- `"yearqtr"` : We build the object using
  [`zoo::yearqtr()`](https://rdrr.io/pkg/zoo/man/yearqtr.html) on a
  double in the format `2000.5`

- `"next"` : Use the constructor for the next supported class.
