
<!-- README.md is generated from README.Rmd. Please edit that file -->
# constructive

{constructive} prints code that can be used to recreate R objects. In a
sense it is similar to `base::dput()` but {constructive} strives to use
“natural” constructors (`factor` for factors, `as.Date()` for dates,
`data.frame()` for data frames etc), in order to get output readable by
humans.

Some use cases :

-   snapshot test
-   Exploring objects (alternative to `dput()` or `str()`)
-   Creating reproducible examples from existing data
-   Comparing two objects (using `construct_diff()`)

## Installation

Install with:

    remotes::install_github("cynkra/constructive")

## Examples

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

library(dplyr, warn = F)
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

We can provide to the `data`argument a list, environment, or package
where to look for data so we don’t print more than necessary, for
example improving the previous example:

``` r
construct(grouped_band_members, data = "dplyr")
#> band_members |>
#>   dplyr::group_by(band)
```

We can also trim the output and display onle `max_atomic` element at
most in a vector :

``` r
construct(list(iris = iris, cars = cars), max_atomic = 3)
#> list(
#>   iris = data.frame(
#>     Sepal.Length = c(5.1, 4.9, 4.7, ...),
#>     Sepal.Width = c(3.5, 3, 3.2, ...),
#>     Petal.Length = c(1.4, 1.4, 1.3, ...),
#>     Petal.Width = c(0.2, 0.2, 0.2, ...),
#>     Species = factor(c("setosa", "setosa", "setosa", ...))
#>   ),
#>   cars = data.frame(speed = c(4, 4, 7, ...), dist = c(2, 10, 4, ...))
#> )
```

If we use `max_atomic = 0` we build a prototype :

``` r
construct(list(iris = iris, cars = cars), max_atomic = 0)
#> list(
#>   iris = data.frame(
#>     Sepal.Length = numeric(0),
#>     Sepal.Width = numeric(0),
#>     Petal.Length = numeric(0),
#>     Petal.Width = numeric(0),
#>     Species = factor(character(0))
#>   ),
#>   cars = data.frame(speed = numeric(0), dist = numeric(0))
#> )
```

Some other options :

``` r
construct(band_members, tribble = TRUE)
#> tibble::tribble(
#>   ~name,  ~band,
#>   "Mick", "Stones",
#>   "John", "Beatles",
#>   "Paul", "Beatles",
#> )
construct(as.data.frame(band_members), read.table = TRUE)
#> read.table(header = TRUE, text = "
#> name band
#> Mick Stones
#> John Beatles
#> Paul Beatles
#> ")
```

## Limitations

Environments are not always possible to reproduce but we support some
common cases. Due to this several objects such as formulas, srcrefs, R6
objects, ggplot objects etc might not be reproducible exactly. If an
approximation is enough you might set `check = FALSE`,
`ignore_srcref = TRUE`, `env_as_list = FALSE`.

## construct_diff

An alternative to `waldo::compare()` (looks best in the IDE without
`interactive = FALSE`)

``` r
# The args max_atomic, max_boy and env_as_list can be used to reduce output,
# here we want to see what adding geom_point() does to a ggplot
library(ggplot2)
construct_diff(
  # max_atomic limits number of displayed elements for atomic vectors
  max_atomic = 4, 
  # if env_as_list is FALSE instead of defining unnamed envs as 
  # `as.environment(list(elt = ...))` we simply use `new.env()`
  env_as_list = FALSE, 
  interactive = FALSE,
  ggplot(cars, aes(speed, dist)),
  ggplot(cars, aes(speed, dist)) + geom_point()
)
#> < ggplot(cars, aes(speed, dist))         > ggplot(cars, aes(speed, dist)) +   ..
#> @@ 1,5 @@                                @@ 1,8 @@                              
#>   list(                                    list(                                
#>     data = data.frame(speed = c(4, 4, 7      data = data.frame(speed = c(4, 4, 7
#>   , 7, ...), dist = c(2, 10, 4, 22, ...    , 7, ...), dist = c(2, 10, 4, 22, ...
#>   )),                                      )),                                  
#> <   layers = list(),                     >   layers = list(                     
#> ~                                        >     new.env() |>                     
#> ~                                        >       structure(class = c("LayerInsta
#> ~                                        : nce", "Layer", "ggproto", "gg"))     
#> ~                                        >   ),                                 
#>     scales = new.env() |>                    scales = new.env() |>              
#>       structure(class = c("ScalesList",        structure(class = c("ScalesList",
#>    "ggproto", "gg")),                       "ggproto", "gg")),
```
