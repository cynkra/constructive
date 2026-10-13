# Constructive options for class 'raster'

These options will be used on objects of class 'raster'.

## Usage

``` r
opts_raster(constructor = c("as.raster", "next"), ...)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object, see Details
  section.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class \<constructive_options/constructive_options_raster\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"as.raster"` (default): We build the object using
  [`as.raster()`](https://rdrr.io/r/grDevices/as.raster.html) on a
  character matrix of colors.

- `"next"` : Use the constructor for the next supported class. Call
  [`.class2()`](https://rdrr.io/r/base/class.html) on the object to see
  in which order the methods will be tried. This will usually be
  equivalent to `opts_matrix("matrix")`, note that the colors of a
  raster are stored row by row.
