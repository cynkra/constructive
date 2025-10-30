# Constructive options for package_version

Depending on `constructor`, we construct the object as follows:

- `"package_version"` : We use
  [`package_version()`](https://rdrr.io/r/base/numeric_version.html)

- `"next"` : Use the constructor for the next supported class. Call
  [`.class2()`](https://rdrr.io/r/base/class.html) on the object to see
  in which order the methods will be tried. This will usually be
  equivalent to `"array"`

- `"list"` : We define as a list and repair attributes

## Usage

``` r
opts_package_version(constructor = c("package_version", "next", "list"), ...)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class
\<constructive_options/constructive_options_package_version\>
