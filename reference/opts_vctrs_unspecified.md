# Constructive options for class 'vctrs_unspecified'

These options will be used on objects of class 'vctrs_unspecified'.

## Usage

``` r
opts_vctrs_unspecified(constructor = c("unspecified", "next"), ...)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object, see Details
  section.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class
\<constructive_options/constructive_options_vctrs_unspecified\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"unspecified"` (default): Use
  [`vctrs::unspecified()`](https://vctrs.r-lib.org/reference/vctrs-unspecified.html)
  on the length of the object.

- `"next"` : Use the constructor for the next supported class.
