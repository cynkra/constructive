# Insert a pipe between two calls

Exported for custom constructor design.

## Usage

``` r
.cstr_pipe(x, y, ..., pipe = NULL, one_liner = FALSE, indent = TRUE)
```

## Arguments

- x:

  A character vector. The code for the left hand side call.

- y:

  A character vector. The code for the right hand side call.

- ...:

  Implemented to collect unused arguments forwarded by the dots of the
  caller environment.

- pipe:

  A string. The pipe to use, `"plus"` is useful for ggplot code.

- one_liner:

  A boolean. Whether to paste `x`, the pipe and `y` together

- indent:

  A boolean. Whether to indent `y` on a same line (provided that `x` and
  `y` are strings and one liners themselves)

## Value

A character vector

## Examples

``` r
.cstr_pipe("iris", "head(2)", pipe = "magrittr", one_liner = FALSE)
#> [1] "iris %>%"  "  head(2)"
.cstr_pipe("iris", "head(2)", pipe = "magrittr", one_liner = TRUE)
#> [1] "iris %>% head(2)"
```
