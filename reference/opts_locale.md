# Constructive options for class 'locale'

These options will be used on objects of class 'locale'.

## Usage

``` r
opts_locale(constructor = c("locale", "next", "list"), ...)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class \<constructive_options/constructive_options_locale\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"locale"` (default): We build the object using
  [`readr::locale()`](https://readr.tidyverse.org/reference/locale.html),
  providing only non default arguments. `date_names` is provided as a
  language code when possible.

- `"next"` : Use the constructor for the next supported class. Call
  [`.class2()`](https://rdrr.io/r/base/class.html) on the object to see
  in which order the methods will be tried. This will usually be
  equivalent to `"list"`

- `"list"` : We define as a list and repair attributes

Use
[`opts_date_names()`](https://cynkra.github.io/constructive/reference/opts_date_names.md)
to tweak the construction of the `date_names` element,
`opts_date_names("date_names")` will prevent the use of a language code.
