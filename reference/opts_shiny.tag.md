# Constructive options for class 'shiny.tag'

These options will be used on objects of class 'shiny.tag'.

## Usage

``` r
opts_shiny.tag(constructor = c("tags", "tag", "next"), ...)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class
\<constructive_options/constructive_options_shiny.tag\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"tags"` (default): We build the object using
  `htmltools::tags$<name>()`, e.g. `htmltools::tags$div()`, with
  attributes as named arguments and children as unnamed arguments. If
  the tag name is not a known tag or if some attributes would clash with
  the formal arguments of the tag function, we fall back to the `"tag"`
  constructor.

- `"tag"` : We build the object using
  [`htmltools::tag()`](https://rstudio.github.io/htmltools/reference/builder.html).
  [`htmltools::tag()`](https://rstudio.github.io/htmltools/reference/builder.html)
  can't create an empty named list of attributes (as
  `htmltools::tags$div()` does), in this case we fall back to the
  `"tags"` constructor for known tag names, and to the list constructor
  otherwise.

- `"next"` : Use the constructor for the next supported class.
