# Constructive options for class 'R6ClassGenerator'

These options will be used on objects of class 'R6ClassGenerator'.

## Usage

``` r
opts_R6ClassGenerator(constructor = c("R6Class", "next"), ...)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class
\<constructive_options/constructive_options_R6ClassGenerator\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"R6Class"` (default): We build the object using `R6Class()`.

- `"next"` : Use the constructor for the next supported class.
