# Print code with syntax highlighting

Print code with syntax highlighting

## Usage

``` r
# S3 method for class 'constructive_code'
print(
  x,
  ...,
  colored = getOption("constructive_pretty", TRUE),
  code_theme = NULL,
  style = NULL
)
```

## Arguments

- x:

  The object to print

- ...:

  Unused

- colored:

  Whether to apply syntax highlighting. Set to `FALSE`, or use
  `options(constructive_pretty = FALSE)` to turn off highlighting.

- code_theme:

  Syntax highlighting theme passed to
  [`cli::code_highlight()`](https://cli.r-lib.org/reference/code_highlight.html).
  Setting `code_theme = list()` will remove all syntax highlighting, but
  hyperlinks will remain if supported.

- style:

  Deprecated in favour of `code_theme`
