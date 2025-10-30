# Constructive options for class 'Layer' (ggplot2)

These options will be used on objects of class 'Layer'.

## Usage

``` r
opts_Layer(constructor = c("default", "layer", "next", "environment"), ...)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object, see Details
  section.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class \<constructive_options/constructive_options_Layer\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"default"` : We attempt to use the function originally used to create
  the plot.

- `"layer"` : We use the
  [`ggplot2::layer()`](https://ggplot2.tidyverse.org/reference/layer.html)
  function

- `"environment"` : Reconstruct the object using the general environment
  method (which can be itself tweaked using
  [`opts_environment()`](https://cynkra.github.io/constructive/reference/opts_environment.md))

The latter constructor is the only one that reproduces the object
exactly since Layers are environments and environments can't be exactly
copied (see
[`?opts_environment`](https://cynkra.github.io/constructive/reference/opts_environment.md))
