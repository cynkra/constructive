# Constructive options for class 'date_names'

These options will be used on objects of class 'date_names'.

## Usage

``` r
opts_date_names(
  constructor = c("date_names_lang", "date_names", "next", "list"),
  ...
)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class
\<constructive_options/constructive_options_date_names\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"date_names_lang"` (default): We build the object using
  [`readr::date_names_lang()`](https://readr.tidyverse.org/reference/date_names.html)
  if the object matches one of the languages listed by
  [`readr::date_names_langs()`](https://readr.tidyverse.org/reference/date_names.html),
  and fall back to the `"date_names"` constructor otherwise.

- `"date_names"` : We build the object using
  [`readr::date_names()`](https://readr.tidyverse.org/reference/date_names.html),
  providing only non default arguments.

- `"next"` : Use the constructor for the next supported class. Call
  [`.class2()`](https://rdrr.io/r/base/class.html) on the object to see
  in which order the methods will be tried. This will usually be
  equivalent to `"list"`

- `"list"` : We define as a list and repair attributes
