# Extend constructive

``` r
library(constructive)
```

We detail in this vignette how {constructive} works and how you might
extend it by defining custom `.cstr_construct.<class>()` and
`.cstr_construct.<class>.<constructor>()` methods.

The functions
[`.cstr_new_class()`](https://cynkra.github.io/constructive/reference/templates.md)
and
[`.cstr_new_constructor()`](https://cynkra.github.io/constructive/reference/templates.md)
can be used to create custom constructors from templates. The easiest
workflow is probably to take a look at the package
[{constructive.examples}](https://github.com/cynkra/constructive.example/)
which will guide you through the process, then call these functions with
the argument `commented = TRUE`.

If this is not enough, or if you want to know more, we describe below in
more details how the package and its key functions work.

## `construct()` and `.cstr_construct()`

- [`.cstr_construct()`](https://cynkra.github.io/constructive/reference/dot-cstr_construct.md)
  builds code recursively, without checking the inputs or output
  validity, without error handling, and without formatting.
- [`construct()`](https://cynkra.github.io/constructive/reference/construct.md)
  wraps
  [`.cstr_construct()`](https://cynkra.github.io/constructive/reference/dot-cstr_construct.md)
  and does this extra work.
- [`.cstr_construct()`](https://cynkra.github.io/constructive/reference/dot-cstr_construct.md)
  is a generic and many methods are implemented in the package, for
  instance `construct(iris)` will call `.cstr_construct.data.frame()`
  which down the line will call `.cstr_construct.numeric()` and
  `.cstr_construct.factor()` to construct its columns.
- [`.cstr_construct()`](https://cynkra.github.io/constructive/reference/dot-cstr_construct.md)
  contains extra logic to :
  - Attempt to match its data input to a list of objects provided to the
    `data` argument.
  - Restrict the dispatch so the `classes` argument and the functions
    [`construct_base()`](https://cynkra.github.io/constructive/reference/construct_dput.md)
    and
    [`construct_dput()`](https://cynkra.github.io/constructive/reference/construct_dput.md)
    can work.

``` r
.cstr_construct
#> function(x, ..., data = NULL, classes = NULL) {
#>   data_name <- perfect_match(x, data)
#>   if (!is.null(data_name)) return(data_name)
#>   if (is.null(classes)) {
#>     UseMethod(".cstr_construct")
#>   } else if (identical(classes, "-")) {
#>     .cstr_construct.default(x, ..., classes = classes)
#>   } else if (classes[[1]] == "-") {
#>     cl <- setdiff(.class2(x), classes[-1])
#>     UseMethod(".cstr_construct", structure(NA_integer_, class = cl))
#>   } else {
#>     cl <- intersect(.class2(x), classes)
#>     UseMethod(".cstr_construct", structure(NA_integer_, class = cl))
#>   }
#> }
#> <bytecode: 0x5575b4a4d398>
#> <environment: namespace:constructive>
# a character vector
.cstr_construct(letters)
#> [1] "c("                                                                                                        
#> [2] "  \"a\", \"b\", \"c\", \"d\", \"e\", \"f\", \"g\", \"h\", \"i\", \"j\", \"k\", \"l\", \"m\", \"n\", \"o\","
#> [3] "  \"p\", \"q\", \"r\", \"s\", \"t\", \"u\", \"v\", \"w\", \"x\", \"y\", \"z\""                             
#> [4] ")"
# a constructive object, 
construct(letters)
#> c(
#>   "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o",
#>   "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"
#> )
```

## `.cstr_construct.<class>()` methods

`.cstr_construct.<class>()` methods are generics themselves, they
typically have this form:

``` r
.cstr_construct.Date <- function(x, ...) {
  opts <- list(...)$opts$Date %||% opts_Date()
  if (is_corrupted_Date(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.Date", structure(NA, class = opts$constructor))
}
```

- `opts` contains options passed to the `...` or `template` arguments of
  [`construct()`](https://cynkra.github.io/constructive/reference/construct.md)
  through the `opts_*()` functions, we fall back to a default value if
  none were provided. The `list(...)$opts` idiom is used in various
  places, it allows us to forward the dots conveniently.
- If the object is corrupted or if the user decided to bypass the
  current method by choosing “next” as a constructor for this class, we
  return [`NextMethod()`](https://rdrr.io/r/base/UseMethod.html) to
  forward all our inputs to a lower level constructor.
- We finally dispatch to a method based on the constructor, and not on
  the object’s class as is done traditionally. This explains the unusual
  look of the [`UseMethod()`](https://rdrr.io/r/base/UseMethod.html)
  call above.

## `opts_<class>()` function

When implementing a new method you’ll need to define and export the
corresponding `opts_<class>()` function. It provides to the user with a
way to choose a constructor and additional parameters, and sets the
default behavior.

It should always have this form:

``` r
opts_Date <- function(
    constructor = c(
      "as.Date", "as_date", "date", "new_date", "as.Date.numeric", "as_date.numeric", "next", "double"), 
    ..., 
    origin = "1970-01-01") {
  .cstr_options("Date", constructor = constructor[[1]], ..., origin = origin)
}
```

- The class is present in the name of the `opts_<class>()` function and
  as the first argument of
  [`.cstr_options()`](https://cynkra.github.io/constructive/reference/dot-cstr_options.md).
- A character vector of constructors is provided, starting with the
  default constructor
- A “next” value is mandatory except for internal types.
- Additional options retrievable by the constructor might be added, as
  we did here with `origin`
- Note that we don’t use
  [`match.arg()`](https://rdrr.io/r/base/match.arg.html) here, because
  new constructors can be defined outside of the package.

## `is_corrupted_<class>()` function

`is_corrupted_<class>()` checks if `x` has the right internal type and
attributes, sometimes structure, so that it satisfies the expectations
of a well formatted object of a given class.

If an object is corrupted for a given class we cannot use constructors
for this class, so we move on to a lower level constructor by calling
[`NextMethod()`](https://rdrr.io/r/base/UseMethod.html) in
[`.cstr_construct()`](https://cynkra.github.io/constructive/reference/dot-cstr_construct.md).

This is important so that
[constructive](https://github.com/cynkra/constructive) doesn’t choke on
corrupted objects but instead helps us understand them.

For instance in the following example `x` prints like a date but it’s
corrupted, a date should not be built on top of characters and this
object cannot be built with
[`as.Date()`](https://rdrr.io/r/base/as.Date.html) or other idiomatic
date constructors.

``` r
x <- structure("12345", class = "Date")
x
#> [1] "2003-10-20"
x + 1
#> Error in unclass(e1) + unclass(e2): non-numeric argument to binary operator
```

We have defined :

``` r
is_corrupted_Date <- function(x) {
  !is.double(x)
}
```

And as a consequence the next method, `.cstr_construct.default()` will
be called through
[`NextMethod()`](https://rdrr.io/r/base/UseMethod.html) and will handle
the object using an atomic vector constructor:

``` r
construct(x)
#> "12345" |>
#>   structure(class = "Date")
```

## Constructors

constructors are functions named as
`.cstr_construct.<class>.<constructor>`.

For instance the default constructor for “Date” is :

``` r
constructive:::.cstr_construct.Date.as.Date
#> function(x, ...) {
#>   opts <- list(...)$opts$Date %||% opts_Date()
#>   origin <- opts$origin
#>   compatible_with_char <-
#>     all(rlang::is_integerish(x) & (is.finite(x) | (is.na(x) & !is.nan(x))))
#>   if (!compatible_with_char || all(is.na(x))) {
#>     return(.cstr_construct.Date.as.Date.numeric(x, ...))
#>   }
#>   code <- .cstr_apply(list(format(x)),  "as.Date", ..., new_line = FALSE)
#>   repair_attributes_Date(x, code, ...)
#> }
#> <bytecode: 0x5575b4c58778>
#> <environment: namespace:constructive>
```

Their arguments are `x` and `...`, and not more. Additional parameters
fed to the `opt_<class>()` function can be fetched from
`list(...)$opts$<class>`

The function
[`.cstr_apply()`](https://cynkra.github.io/constructive/reference/dot-cstr_apply.md)
is used to construct arguments recursively.

Sometimes a constructor cannot handle all cases and we need to fall back
to another constructor, it happens below because `Inf`, `NA`, or decimal
dates cannot be represented by a string wrapped by
[`as.Date()`](https://rdrr.io/r/base/as.Date.html).

``` r
x <- structure(c(12345, 20000), class = "Date")
y <- structure(c(12345, Inf), class = "Date")
construct(x)
#> as.Date(c("2003-10-20", "2024-10-04"))
construct(y)
#> as.Date(c(12345, Inf), origin = "1970-01-01")
```

That last line of the function is essential, it does the attribute
repair.

## Attribute repair

Constructors should always end by a call to
[`.cstr_repair_attributes()`](https://cynkra.github.io/constructive/reference/dot-cstr_repair_attributes.md)
or a function that wraps it.

These are needed to adjust the attributes of an object after idiomatic
constructors such as [`as.Date()`](https://rdrr.io/r/base/as.Date.html)
have defined their data and canonical attributes.

``` r
x <- structure(c(12345, 20000), class = "Date", some_attr = 42)
# attributes are not visible due to "Date"'s printing method
x
#> [1] "2003-10-20" "2024-10-04"

construct(x)
#> as.Date(c("2003-10-20", "2024-10-04")) |>
#>   structure(some_attr = 42)
```

[`.cstr_repair_attributes()`](https://cynkra.github.io/constructive/reference/dot-cstr_repair_attributes.md)
essentially sets attributes with exceptions :

- It doesn’t set names by default, these should be handled by the
  constructors
- It doesn’t set the class explicitly if it’s identical to the idiomatic
  class, i.e. the class returned by the constructor before the repair
  call, and provided through the `idiomatic_class` argument
- It doesn’t set attributes that we choose to ignore because they are
  set by the constructor (e.g. row names for data frames or levels for
  factors)

``` r
constructive:::repair_attributes_Date
#> function(x, code, ...) {
#>   .cstr_repair_attributes(
#>     x, code, ...,
#>     idiomatic_class = "Date"
#>   )
#> }
#> <bytecode: 0x5575b5f2b340>
#> <environment: namespace:constructive>

constructive:::repair_attributes_factor
#> function(x, code, ...) {
#>   .cstr_repair_attributes(
#>     x, code, ...,
#>     ignore = "levels",
#>     idiomatic_class = "factor"
#>   )
#> }
#> <bytecode: 0x5575b4a23660>
#> <environment: namespace:constructive>

constructive:::repair_attributes_tbl_df
#> function(x, code, ...) {
#>   .cstr_repair_attributes(
#>     x, code, ...,
#>     ignore = "row.names",
#>     idiomatic_class = c("tbl_df", "tbl", "data.frame")
#>   )
#> }
#> <bytecode: 0x5575b49bc418>
#> <environment: namespace:constructive>
```
