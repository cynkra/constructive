# Constructive options for class 'sfg'

These options will be used on objects of class 'sfg', simple feature
geometries from the 'sf' package.

## Usage

``` r
opts_sfg(constructor = c("st_sfg", "next"), ...)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object, see Details
  section.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class \<constructive_options/constructive_options_sfg\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"st_sfg"` (default): We build the object using the sf constructor
  matching the geometry type, i.e.
  [`sf::st_point()`](https://r-spatial.github.io/sf/reference/st.html),
  [`sf::st_multipoint()`](https://r-spatial.github.io/sf/reference/st.html),
  [`sf::st_linestring()`](https://r-spatial.github.io/sf/reference/st.html),
  [`sf::st_multilinestring()`](https://r-spatial.github.io/sf/reference/st.html),
  [`sf::st_polygon()`](https://r-spatial.github.io/sf/reference/st.html),
  [`sf::st_multipolygon()`](https://r-spatial.github.io/sf/reference/st.html)
  or
  [`sf::st_geometrycollection()`](https://r-spatial.github.io/sf/reference/st.html),
  with a `dim` argument when the dimension can't be inferred from the
  coordinates.

- `"next"` : Use the constructor for the next supported class. Call
  [`.class2()`](https://rdrr.io/r/base/class.html) on the object to see
  in which order the methods will be tried.

Other geometry types, such as "CIRCULARSTRING", don't have a dedicated
constructor and are built with the next constructor.
