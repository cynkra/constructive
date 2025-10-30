# Constructive options for type 'language'

These options will be used on objects of type 'language'. By default
this function is useless as nothing can be set, this is provided in case
users want to extend the method with other constructors.

## Usage

``` r
opts_language(constructor = c("default"), ...)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class
\<constructive_options/constructive_options_language\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"default"` : We use constructive's deparsing algorithm on
  attributeless calls, and use
  [`as.call()`](https://rdrr.io/r/base/call.html) on other language
  elements when attributes need to be constructed.
