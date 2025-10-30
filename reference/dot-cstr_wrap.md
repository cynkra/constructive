# Wrap argument code in function call

Exported for custom constructor design. Generally called through
[`.cstr_apply()`](https://cynkra.github.io/constructive/reference/dot-cstr_apply.md).

## Usage

``` r
.cstr_wrap(args, fun, new_line = FALSE)
```

## Arguments

- args:

  A character vector containing the code of arguments.

- fun:

  A string. The name of the function to use in the function call. Use
  `fun = ""` to wrap in parentheses.

- new_line:

  Boolean. Whether to insert a new line between `"fun("` and the closing
  `")"`.

## Value

A character vector.
