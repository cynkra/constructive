
<!-- README.md is generated from README.Rmd. Please edit that file -->
# constructive

{constructive} prints code that can be used to recreate R objects. In a
sense it is similar to `base::dput()` or `base::deparse()` but
{constructive} strives to use “natural” constructors (`factor` for
factors, `as.Date()` for dates, `data.frame()` for data frames etc), in
order to get output readable by humans.

Some use cases are:

-   Snapshot tests
-   Exploring objects (alternative to `dput()` or `str()`)
-   Creating reproducible examples from existing data
-   Comparing two objects (using `construct_diff()`)

## Installation

Install with:

    remotes::install_github("cynkra/constructive")

## Comparison with `dput()`

A few examples compared to their `dput()` output.

``` r
library(constructive)

construct(head(iris, 2))
#> data.frame(
#>   Sepal.Length = c(5.1, 4.9),
#>   Sepal.Width = c(3.5, 3),
#>   Petal.Length = c(1.4, 1.4),
#>   Petal.Width = c(0.2, 0.2),
#>   Species = factor(c("setosa", "setosa"), levels = c("setosa", "versicolor", "virginica"))
#> )

dput(head(iris, 2))
#> structure(list(Sepal.Length = c(5.1, 4.9), Sepal.Width = c(3.5, 
#> 3), Petal.Length = c(1.4, 1.4), Petal.Width = c(0.2, 0.2), Species = structure(c(1L, 
#> 1L), .Label = c("setosa", "versicolor", "virginica"), class = "factor")), row.names = 1:2, class = "data.frame")

construct(.leap.seconds)
#> as.POSIXct(
#>   c(
#>     "1972-07-01", "1973-01-01", "1974-01-01", "1975-01-01", "1976-01-01",
#>     "1977-01-01", "1978-01-01", "1979-01-01", "1980-01-01", "1981-07-01",
#>     "1982-07-01", "1983-07-01", "1985-07-01", "1988-01-01", "1990-01-01",
#>     "1991-01-01", "1992-07-01", "1993-07-01", "1994-07-01", "1996-01-01",
#>     "1997-07-01", "1999-01-01", "2006-01-01", "2009-01-01", "2012-07-01",
#>     "2015-07-01", "2017-01-01"
#>   ),
#>   tz = "GMT"
#> )

dput(.leap.seconds)
#> structure(c(78796800, 94694400, 126230400, 157766400, 189302400, 
#> 220924800, 252460800, 283996800, 315532800, 362793600, 394329600, 
#> 425865600, 489024000, 567993600, 631152000, 662688000, 709948800, 
#> 741484800, 773020800, 820454400, 867715200, 915148800, 1136073600, 
#> 1230768000, 1341100800, 1435708800, 1483228800), class = c("POSIXct", 
#> "POSIXt"), tzone = "GMT")

library(dplyr, warn.conflicts = FALSE)
grouped_band_members <- group_by(band_members, band)

dput(grouped_band_members)
#> structure(list(name = c("Mick", "John", "Paul"), band = c("Stones", 
#> "Beatles", "Beatles")), class = c("grouped_df", "tbl_df", "tbl", 
#> "data.frame"), row.names = c(NA, -3L), groups = structure(list(
#>     band = c("Beatles", "Stones"), .rows = structure(list(2:3, 
#>         1L), ptype = integer(0), class = c("vctrs_list_of", "vctrs_vctr", 
#>     "list"))), class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA, 
#> -2L), .drop = TRUE))

construct(grouped_band_members)
#> tibble::tibble(name = c("Mick", "John", "Paul"), band = c("Stones", "Beatles", "Beatles")) |>
#>   dplyr::group_by(band)
```

We can provide to the `data` argument a named list, an environment, a
package where to look for data, or an unnamed list of such items, so we
don’t print more than necessary, for instance improving the previous
example:

``` r
construct(grouped_band_members, data = "dplyr")
#> band_members |>
#>   dplyr::group_by(band)
```

## customize output with constructive options

Many classes or types can be represented in various ways, for instance a
tibble might be constructed using `tibble::tibble()` or using
`tibble::tribble()`.

The `opts_*()` family of functions is used for this purpose. These
functions are used to provide unnamed arguments to `construct()`, for
instance :

``` r
construct(band_members, opts_tbl_df("tribble"))
#> tibble::tribble(
#>   ~name,  ~band,
#>   "Mick", "Stones",
#>   "John", "Beatles",
#>   "Paul", "Beatles",
#> )
```

These functions have their own documentation page and are referenced in
`?construct`.

In particular `opts_list()`, `opts_atomic()` and `opts_function()` have
a `trim` argument that might be used to show a terser output. In this
case obviously the output does not faithfully reproduces the input, and
in some cases it is not even possible to evaluate it.

``` r
construct(starwars, opts_list(trim = 1), opts_atomic(trim = 1))
#> {constructive} couldn't create code that reproduces perfectly the input
#> ℹ Call `construct_issues()` to inspect the last issues
#> tibble::tibble(
#>   name = c("Luke Skywalker", character(86)),
#>   height = c(172L, numeric(86)),
#>   mass = c(77, numeric(86)),
#>   hair_color = c("blond", character(86)),
#>   skin_color = c("fair", character(86)),
#>   eye_color = c("blue", character(86)),
#>   birth_year = c(19, numeric(86)),
#>   sex = c("male", character(86)),
#>   gender = c("masculine", character(86)),
#>   homeworld = c("Tatooine", character(86)),
#>   species = c("Human", character(86)),
#>   films = c(list(c("The Empire Strikes Back", character(4))), vector("list", 86)),
#>   vehicles = c(list(c("Snowspeeder", character(1))), vector("list", 86)),
#>   starships = c(list(c("X-wing", character(1))), vector("list", 86)),
#> )

construct(starwars, opts_list(trim = 1, fill = "..."), opts_atomic(trim = 1, fill = "..."))
#> ! The code built by {constructive} could not be evaluated.
#> tibble::tibble(
#>   name = c("Luke Skywalker", ...),
#>   height = c(172L, ...),
#>   mass = c(77, ...),
#>   hair_color = c("blond", ...),
#>   skin_color = c("fair", ...),
#>   eye_color = c("blue", ...),
#>   birth_year = c(19, ...),
#>   sex = c("male", ...),
#>   gender = c("masculine", ...),
#>   homeworld = c("Tatooine", ...),
#>   species = c("Human", ...),
#>   films = list(c("The Empire Strikes Back", ...), ...),
#>   vehicles = list(c("Snowspeeder", ...), ...),
#>   starships = list(c("X-wing", ...), ...),
#> )
```

## Environments

Some special environments can be reproduced perfectly:

``` r
construct(globalenv())
#> .GlobalEnv

construct(environment(setNames))
#> asNamespace("stats")
```

In general however, because environments use reference semantics, they
cannot be copied. An attempt to copy an environment would indeed yield a
different environment and `identical(env, copy)` would be `FALSE` (read
more about it in `?opts_environment`).

In practice however 2 environments containing the same values and having
the same parent will be equivalent. When {constructive} can reproduce
those, it won’t complain about any difference.

``` r
e1 <- new.env(parent = .GlobalEnv)
e1$x <- 1
construct(e1)
#> list2env(list(x = 1), parent = .GlobalEnv)
```

However in the general case we’d have to construct all parents too and
it might get verbose, by default {constructive} doesn’t go all the way.

``` r
e2 <- new.env(parent = e1)
e2$y <- 2
construct(e2)
#> {constructive} couldn't create code that reproduces perfectly the input
#> ℹ Call `construct_issues()` to inspect the last issues
#> list2env(list(y = 2), parent = .GlobalEnv)
```

However if we set `recurse` to `TRUE` we’ll get an equivalent
environment :

``` r
construct(e2, opts_environment(recurse = TRUE), pipe = "magrittr")
#> .GlobalEnv %>%
#>   list2env(list(x = 1), parent = .) %>%
#>   list2env(list(y = 2), parent = .)
```

See `?opts_environment` for more.

## Functions

Functions have en environment, which is not set by default. We might set
it explicitly, or we might also not check for equivalence of function
environments.

``` r
construct(setNames)
#> {constructive} couldn't create code that reproduces perfectly the input
#> ℹ Call `construct_issues()` to inspect the last issues
#> function(object = nm, nm) {
#>   names(object) <- nm
#>   object
#> }

construct(setNames, opts_function(environment = TRUE))
#> (function(object = nm, nm) {
#>   names(object) <- nm
#>   object
#> }) |>
#>   match.fun("environment<-")(asNamespace("stats"))

construct(setNames, ignore_function_env = TRUE)
#> function(object = nm, nm) {
#>   names(object) <- nm
#>   object
#> }
```

## construct_diff

`construct_diff()` highlights the differences in the code used to
produce 2 objects.

``` r
construct_diff(
  list(a = head(cars,2), b = "aaaaaaaaaaaaaaaaaaaa", c = "Foo"),
  list(a = head(iris,1), b = "aaaaaaaaaaaaaaaaaaaa", c = "foo")
)
```

<!-- ![](man/figures/construct_diff.png) -->
