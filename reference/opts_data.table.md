# Constructive options for class 'data.table'

These options will be used on objects of class 'data.table'.

## Usage

``` r
opts_data.table(
  constructor = c("data.table", "next", "list"),
  ...,
  selfref = FALSE,
  recycle = TRUE
)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object, see Details
  section.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

- selfref:

  Boolean. Whether to include the `.internal.selfref` attribute. It's
  probably not useful, hence the default,
  [`waldo::compare()`](https://waldo.r-lib.org/reference/compare.html)
  is used to assess the output fidelity and doesn't check it, but if you
  really need to generate code that builds an object
  [`identical()`](https://rdrr.io/r/base/identical.html) to the input
  you'll need to set this to `TRUE`.#'

- recycle:

  Boolean. Whether to recycle scalars to compress the output.

## Value

An object of class
\<constructive_options/constructive_options_data.table\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"data.table"` (default): Wrap the column definitions in a
  `data.table()` call.

- `"next"` : Use the constructor for the next supported class. Call
  [`.class2()`](https://rdrr.io/r/base/class.html) on the object to see
  in which order the methods will be tried.

- `"list"` : Use [`list()`](https://rdrr.io/r/base/list.html) and treat
  the class as a regular attribute.
