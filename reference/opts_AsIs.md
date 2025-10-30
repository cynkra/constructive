# Constructive options for the class `AsIs`

These options will be used on objects of class `AsIs`. `AsIs` objects
are created with [`I()`](https://rdrr.io/r/base/AsIs.html) which only
prepends `"AsIs"` to the class attribute.

## Usage

``` r
opts_AsIs(constructor = c("I", "next"), ...)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object, see Details
  section.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class \<constructive_options/constructive_options_AsIs\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"I"` (default): Use the [`I()`](https://rdrr.io/r/base/AsIs.html)
  function

- `"next"` : Use the constructor for the next supported class. Call
  [`.class2()`](https://rdrr.io/r/base/class.html) on the object to see
  in which order the methods will be tried.
