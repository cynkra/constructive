# Constructive options for class 'xts'

These options will be used on objects of class 'xts'.

## Usage

``` r
opts_xts(constructor = c("as.xts.matrix", "next"), ...)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class \<constructive_options/constructive_options_xts\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"as.xts.matrix"` (default): We build the object using
  [`xts::as.xts.matrix()`](https://rdrr.io/pkg/xts/man/as.xts.html).

- `"as.xts.data.frame"`: We build the object using
  [`xts::as.xts.data.frame()`](https://rdrr.io/pkg/xts/man/as.xts.html),
  this is probably the most readable option but couldn't be made the
  default constructor because it requires the 'xts' package to be
  installed .

- `"xts"`: We build the object using
  [`xts::xts()`](https://rdrr.io/pkg/xts/man/xts.html).

- `".xts"`: We build the object using
  [`xts::.xts()`](https://rdrr.io/pkg/xts/man/xts.html).

- `"next"`: Use the constructor for the next supported class.
