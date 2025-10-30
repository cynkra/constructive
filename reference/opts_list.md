# Constructive options for type 'list'

These options will be used on objects of type 'list'.

## Usage

``` r
opts_list(
  constructor = c("list", "list2"),
  ...,
  trim = NULL,
  fill = c("vector", "new_list", "+", "...", "none")
)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object, see Details
  section.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

- trim:

  `NULL` or integerish. Maximum of elements showed before it's trimmed.
  Note that it will necessarily produce code that doesn't reproduce the
  input. This code will parse without failure but its evaluation might
  fail.

- fill:

  String. Method to use to represent the trimmed elements.

## Value

An object of class \<constructive_options/constructive_options_list\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"list"` (default): Build the object by calling
  [`list()`](https://rdrr.io/r/base/list.html).

- `"list2"`: Build the object by calling
  [`rlang::list2()`](https://rlang.r-lib.org/reference/list2.html), the
  only difference with the above is that we keep a trailing comma when
  the list is not trimmed and the call spans several lines.

If `trim` is provided, depending on `fill` we will present trimmed
elements as followed:

- `"vector"` (default): Use
  [`vector()`](https://rdrr.io/r/base/vector.html), so for instance
  `list("a", "b", "c")` might become `c(list("a"), vector("list", 2))`.

- `"new_list"`: Use
  [`rlang::new_list()`](https://rlang.r-lib.org/reference/new-vector.html),
  so for instance `list("a", "b", "c")` might become
  `c(list("a"), rlang::new_list(2))`.

- `"+"`: Use unary `+`, so for instance `list("a", "b", "c")` might
  become `list("a", +2)`.

- `"..."`: Use `...`, so for instance `list("a", "b", "c")` might become
  `list("a", ...)`

- `"none"`: Don't represent trimmed elements.

When `trim` is used the output is parsable but might not be possible to
evaluate, especially with `fill = "..."`. In that case you might want to
set `check = FALSE`
