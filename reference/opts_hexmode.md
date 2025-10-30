# Constructive options for class 'hexmode'

These options will be used on objects of class 'hexmode'.

## Usage

``` r
opts_hexmode(constructor = c("as.hexmode", "next"), ..., integer = FALSE)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

- integer:

  Whether to use [`as.hexmode()`](https://rdrr.io/r/base/hexmode.html)
  on integer rather than character

## Value

An object of class \<constructive_options/constructive_options_hexmode\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"as.hexmode"` (default): We build the object using
  [`as.hexmode()`](https://rdrr.io/r/base/hexmode.html)

- `"next"` : Use the constructor for the next supported class.
