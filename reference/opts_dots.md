# Constructive options for type '...'

These options will be used on objects of type '...'. These are rarely
encountered in practice. By default this function is useless as nothing
can be set, this is provided in case users want to extend the method
with other constructors.

## Usage

``` r
opts_dots(constructor = c("default"), ...)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class \<constructive_options/constructive_options_dots\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"default"` : We use the construct
  `(function(...) get(\"...\"))(a = x, y)` which we evaluate in the
  correct environment.
