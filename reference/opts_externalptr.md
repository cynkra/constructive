# Constructive options for type 'externalptr'

These options will be used on objects of type 'externalptr'. By default
this function is useless as nothing can be set, this is provided in case
users wan to extend the method with other constructors.

## Usage

``` r
opts_externalptr(constructor = c("default"), ...)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class
\<constructive_options/constructive_options_externalptr\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"default"` : We use a special function from the constructive
