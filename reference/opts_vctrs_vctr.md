# Constructive options for class 'vctrs_vctr'

These options will be used on objects of class 'vctrs_vctr'.

## Usage

``` r
opts_vctrs_vctr(constructor = c("new_vctr", "next"), ...)
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
\<constructive_options/constructive_options_vctrs_vctr\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"new_vctr"` (default): Use
  [`vctrs::new_vctr()`](https://vctrs.r-lib.org/reference/new_vctr.html)
  on the underlying vector, additional attributes are passed through
  `...`, subclasses through the `class` argument and `inherit_base_type`
  is set when needed, so this constructor also works for classes built
  on top of 'vctrs_vctr'.

- `"next"` : Use the constructor for the next supported class.
