# Constructive options for the class `weakref`

These options will be used on objects of type `weakref`. `weakref`
objects are rarely encountered and there is no base R function to create
them. However rlang has a `new_weakref` function that we can use.

## Usage

``` r
opts_weakref(constructor = c("new_weakref"), ...)
```

## Arguments

- constructor:

  String. Name of the constructor.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class \<constructive_options/constructive_options_array\>
