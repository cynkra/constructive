# Constructive options for functions

These options will be used on functions, i.e. objects of type "closure",
"special" and "builtin".

## Usage

``` r
opts_function(
  constructor = c("function", "as.function", "new_function"),
  ...,
  environment = TRUE,
  srcref = FALSE,
  trim = NULL
)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object, see Details
  section.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

- environment:

  Boolean. Whether to reconstruct the function's environment.

- srcref:

  Boolean. Whether to attempt to reconstruct the function's srcref.

- trim:

  `NULL` or integerish. Maximum of lines showed in the body before it's
  trimmed, replacing code with `...`. Note that it will necessarily
  produce code that doesn't reproduce the input, but it will parse and
  evaluate without failure.

## Value

An object of class
\<constructive_options/constructive_options_function\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"function"` (default): Build the object using a standard
  `function() {}` definition. This won't set the environment by default,
  unless `environment` is set to `TRUE`. If a srcref is available, if
  this srcref matches the function's definition, and if `trim` is left
  `NULL`, the code is returned from using the srcref, so comments will
  be shown in the output of
  [`construct()`](https://cynkra.github.io/constructive/reference/construct.md).
  In the rare case where the ast body of the function contains non
  syntactic nodes this constructor cannot be used and falls back to the
  `"as.function"` constructor.

- `"as.function"` : Build the object using a
  [`as.function()`](https://rdrr.io/r/base/as.function.html) call. back
  to [`data.frame()`](https://rdrr.io/r/base/data.frame.html).

- `"new_function"` : Build the object using a
  [`rlang::new_function()`](https://rlang.r-lib.org/reference/new_function.html)
  call.
