# Constructive options for class 'bbox'

These options will be used on objects of class 'bbox'.

## Usage

``` r
opts_bbox(constructor = c("st_bbox", "next"), ...)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object, see Details
  section.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class \<constructive_options/constructive_options_bbox\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"st_bbox"` (default): We build the object using
  [`sf::st_bbox()`](https://r-spatial.github.io/sf/reference/st_bbox.html)
  on a named double vector, with a `crs` argument if relevant.

- `"next"` : Use the constructor for the next supported class. Call
  [`.class2()`](https://rdrr.io/r/base/class.html) on the object to see
  in which order the methods will be tried.
