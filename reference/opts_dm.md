# Constructive options class 'dm'

These options will be used on objects of class 'dm'.

## Usage

``` r
opts_dm(constructor = c("dm", "next", "list"), ...)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class \<constructive_options/constructive_options_dm\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"dm"` (default): We use
  [`dm::dm()`](https://dm.cynkra.com/reference/dm.html) and other
  functions from dm to adjust the content. This does not strictly
  reproduce the object because each dm calls creates different `uuid`
  values for its tables, but the recreated object is functionally
  equivalent. Use the "list" constructor for fully accurate
  reconstruction.

- `"next"` : Use the constructor for the next supported class. Call
  [`.class2()`](https://rdrr.io/r/base/class.html) on the object to see
  in which order the methods will be tried.

- `"list"` : Use [`list()`](https://rdrr.io/r/base/list.html) and treat
  the class as a regular attribute.
