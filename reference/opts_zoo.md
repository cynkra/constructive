# Constructive options for class 'zoo'

These options will be used on objects of class 'zoo'.

## Usage

``` r
opts_zoo(constructor = c("zoo", "next"), ...)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class \<constructive_options/constructive_options_zoo\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"zoo"` (default): We build the object using
  [`zoo::zoo()`](https://rdrr.io/pkg/zoo/man/zoo.html).

- `"next"` : Use the constructor for the next supported class.
