# Constructive options for class 'rowwise_df'

These options will be used on objects of class 'rowwise_df'.

## Usage

``` r
opts_rowwise_df(constructor = c("default", "next", "list"), ...)
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
\<constructive_options/constructive_options_rowwise_df\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"next"` : Use the constructor for the next supported class. Call
  [`.class2()`](https://rdrr.io/r/base/class.html) on the object to see
  in which order the methods will be tried.

- `"list"` : We define as an list object and repair attributes.
