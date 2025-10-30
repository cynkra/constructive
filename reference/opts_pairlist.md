# Constructive options for pairlists

Depending on `constructor`, we construct the object as follows:

- `"pairlist"` (default): Build the object using a
  [`pairlist()`](https://rdrr.io/r/base/list.html) call.

- `"pairlist2"` : Build the object using a
  [`rlang::pairlist2()`](https://rlang.r-lib.org/reference/pairlist2.html)
  call.

## Usage

``` r
opts_pairlist(constructor = c("pairlist", "pairlist2"), ...)
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
\<constructive_options/constructive_options_pairlist\>
