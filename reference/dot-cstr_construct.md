# Generic for object code generation

Exported for custom constructor design. `.cstr_construct()` is basically
a naked
[`construct()`](https://cynkra.github.io/constructive/reference/construct.md),
without the checks, the style, the object post processing etc...

## Usage

``` r
.cstr_construct(x, ..., data = NULL, classes = NULL)
```

## Arguments

- x:

  An object, for
  [`construct_multi()`](https://cynkra.github.io/constructive/reference/construct.md)
  a named list or an environment.

- ...:

  Constructive options built with the `opts_*()` family of functions.
  See the "Constructive options" section below.

- data:

  Named list or environment of objects we want to detect and mention by
  name (as opposed to deparsing them further). Can also contain unnamed
  nested lists, environments, or package names, in the latter case
  package exports and datasets will be considered. In case of conflict,
  the last provided name is considered.

- classes:

  A character vector of classes for which to use idiomatic constructors
  when available, we can provide a package instead of all its classes,
  in the "{pkg}" form, and we can use a minus sign (inside the quotes)
  to exclude rather than include. By default we use idiomatic
  constructors whenever possible. The special values `"*none*"` and
  `"*base*"` can be used to restrict the idiomatic construction to the
  objects. See
  [`construct_dput()`](https://cynkra.github.io/constructive/reference/construct_dput.md)
  and
  [`construct_base()`](https://cynkra.github.io/constructive/reference/construct_dput.md)
  for wrappers around this feature.

## Value

A character vector
