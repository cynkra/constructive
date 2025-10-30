# Constructive options for class 'S4'

These options will be used on objects of class 'S4'.

## Usage

``` r
opts_S4(constructor = c("new", "prototype"), ...)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object, see Details
  section.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class \<constructive_options/constructive_options_S4\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"new"` (default): We build the function using `new()` if possible. If
  the class has a "initialize" method we have no practical way to
  reverse-engineer the inputs so we fall back to the "prototype"
  constructor

- `"prototype"` : We start from `getClass("S4")@prototype` and add
  attributes.
