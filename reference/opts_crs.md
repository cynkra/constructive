# Constructive options for class 'crs'

These options will be used on objects of class 'crs'.

## Usage

``` r
opts_crs(constructor = c("st_crs", "next"), ...)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object, see Details
  section.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class \<constructive_options/constructive_options_crs\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"st_crs"` (default): We build the object using
  [`sf::st_crs()`](https://r-spatial.github.io/sf/reference/st_crs.html)
  on an EPSG code if possible (e.g. `sf::st_crs(4326)`), or else on the
  user input (e.g. `sf::st_crs("OGC:CRS84")`) or on the wkt definition,
  the first that reproduces the object exactly.

- `"next"` : Use the constructor for the next supported class. Call
  [`.class2()`](https://rdrr.io/r/base/class.html) on the object to see
  in which order the methods will be tried.

The wkt definition of a crs depends on the installed PROJ version, so a
crs object created on a different setup might not be reproducible with
[`sf::st_crs()`](https://r-spatial.github.io/sf/reference/st_crs.html),
in this case we fall back to the next constructor.
