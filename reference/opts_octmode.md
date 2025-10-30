# Constructive options for class 'octmode'

These options will be used on objects of class 'octmode'.

## Usage

``` r
opts_octmode(constructor = c("as.octmode", "next"), ..., integer = FALSE)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

- integer:

  Whether to use [`as.octmode()`](https://rdrr.io/r/base/octmode.html)
  on integer rather than character

## Value

An object of class \<constructive_options/constructive_options_octmode\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"as.octmode"` (default): We build the object using
  [`as.octmode()`](https://rdrr.io/r/base/octmode.html)

- `"next"` : Use the constructor for the next supported class.
