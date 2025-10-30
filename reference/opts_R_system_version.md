# Constructive options for R_system_version

Depending on `constructor`, we construct the object as follows:

- `"R_system_version"` : We use
  [`R_system_version()`](https://rdrr.io/r/base/numeric_version.html)

- `"next"` : Use the constructor for the next supported class. Call
  [`.class2()`](https://rdrr.io/r/base/class.html) on the object to see
  in which order the methods will be tried. This will usually be
  equivalent to `"list"`

- `"list"` : We define as a list and repair attributes

## Usage

``` r
opts_R_system_version(constructor = c("R_system_version", "next", "list"), ...)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class
\<constructive_options/constructive_options_R_system_version\>
