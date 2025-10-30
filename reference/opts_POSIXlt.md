# Constructive options for class 'POSIXlt'

These options will be used on objects of class 'POSIXlt'.

## Usage

``` r
opts_POSIXlt(constructor = c("as.POSIXlt", "next", "list"), ...)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object, see Details
  section.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class \<constructive_options/constructive_options_POSIXlt\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"as.POSIXlt"` (default): Build the object using a
  [`as.POSIXlt()`](https://rdrr.io/r/base/as.POSIXlt.html) call on a
  character vector.

- `"next"` : Use the constructor for the next supported class. Call
  [`.class2()`](https://rdrr.io/r/base/class.html) on the object to see
  in which order the methods will be tried.

- `"list"` : We define as a list and repair attributes.
