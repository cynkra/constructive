# Constructive options for class 'vctrs_rcrd'

These options will be used on objects of class 'vctrs_rcrd'.

## Usage

``` r
opts_vctrs_rcrd(constructor = c("new_rcrd", "next", "list"), ...)
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
\<constructive_options/constructive_options_vctrs_rcrd\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"new_rcrd"` (default): Use
  [`vctrs::new_rcrd()`](https://vctrs.r-lib.org/reference/new_rcrd.html)
  on the list of fields, additional attributes are passed through `...`
  and subclasses through the `class` argument, so this constructor also
  works for classes built on top of 'vctrs_rcrd'.

- `"next"` : Use the constructor for the next supported class.

- `"list"` : Use [`list()`](https://rdrr.io/r/base/list.html) and treat
  the class as a regular attribute.
