# Extend constructive

`.cstr_new_class()` and `.cstr_new_constructor()` open new unsaved
scripts, optionally commented, that can be used as templates to define
new constructors. If the class is already supported and you want to
implement a new constructor, use `.cstr_new_constructor()`, otherwise
use `.cstr_new_class()`.

## Usage

``` r
.cstr_new_class(
  class = c("CLASS", "PARENT_CLASS"),
  constructor = "PKG::CONSTRUCTOR",
  commented = FALSE
)

.cstr_new_constructor(
  class = c("CLASS", "PARENT_CLASS"),
  constructor = "PKG::CONSTRUCTOR",
  commented = FALSE
)
```

## Arguments

- class:

  Class to support, provide the full
  [`class()`](https://rdrr.io/r/base/class.html) vector.

- constructor:

  Name of the constructor, usually the name of the function you can to
  use to build the object. If not you might need to adjust the script.

- commented:

  Boolean. Whether to include comments in the template.

## Value

Both function return `NULL` invisibly and are called for side effects

## Details

We suggest the following workflow (summarized in a message when you call
the functions):

- Call `usethis::use_package(\"constructive\"`, \\Suggests\\)\` one time
  at any point, this will add a soft dependency on 'constructive' so
  it's only needed to install it when you use it.

- Call `.cstr_new_class()` or `.cstr_new_constructor()`, with
  `commented = TRUE` for more guidance.

- Save the scripts unchanged in the "R" folder of your package.

- `devtools::document()`: this will register the S3 methods.

- Try
  [`construct()`](https://cynkra.github.io/constructive/reference/construct.md)
  on your new object, it should print a call to your chosen constructor.

- Tweak the code, in particular the definition of `args`.

The README of the example extension package
['constructive.example'](https://github.com/cynkra/constructive.example)
guides you through the process. See also {constructive}'s own code and
[`vignette("extend-constructive")`](https://cynkra.github.io/constructive/articles/extend-constructive.md)
for more details.
