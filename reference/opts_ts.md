# Constructive options for time-series objets

Depending on `constructor`, we construct the object as follows:

- `"ts"` : We use [`ts()`](https://rdrr.io/r/stats/ts.html)

- `"next"` : Use the constructor for the next supported class. Call
  [`.class2()`](https://rdrr.io/r/base/class.html) on the object to see
  in which order the methods will be tried. This will usually be
  equivalent to `"atomic"`

- `"atomic"` : We define as an atomic vector and repair attributes

## Usage

``` r
opts_ts(constructor = c("ts", "next", "atomic"), ...)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class \<constructive_options/constructive_options_ts\>
