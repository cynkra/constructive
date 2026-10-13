# Constructive options for class 'roman'

These options will be used on objects of class 'roman'.

## Usage

``` r
opts_roman(constructor = c("as.roman", "next"), ...)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object, see Details
  section.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class \<constructive_options/constructive_options_roman\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"as.roman"` (default): We build the object using
  [`as.roman()`](https://rdrr.io/r/utils/roman.html) on a character
  vector of roman numerals.

- `"next"` : Use the constructor for the next supported class. Call
  [`.class2()`](https://rdrr.io/r/base/class.html) on the object to see
  in which order the methods will be tried. This will usually be
  equivalent to
  [`opts_integer()`](https://cynkra.github.io/constructive/reference/opts_integer.md).
