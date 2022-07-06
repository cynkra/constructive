
<!-- README.md is generated from README.Rmd. Please edit that file -->

# constructive

<!-- badges: start -->
<!-- badges: end -->

`dput()` but better

## Examples

``` r
library(constructive)

foo <- data.frame(
  a = factor(month.abb),
  b = as.Date(sprintf("2022-%s-01", 1:12))
)

dput(foo)
#> structure(list(a = structure(c(5L, 4L, 8L, 1L, 9L, 7L, 6L, 2L, 
#> 12L, 11L, 10L, 3L), .Label = c("Apr", "Aug", "Dec", "Feb", "Jan", 
#> "Jul", "Jun", "Mar", "May", "Nov", "Oct", "Sep"), class = "factor"), 
#>     b = structure(c(18993, 19024, 19052, 19083, 19113, 19144, 
#>     19174, 19205, 19236, 19266, 19297, 19327), class = "Date")), class = "data.frame", row.names = c(NA, 
#> -12L))

construct(foo)
#> data.frame(
#>   a = factor(c(
#>     "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
#>     "Oct", "Nov", "Dec"
#>   )),
#>   b = as.Date(c(
#>     "2022-01-01", "2022-02-01", "2022-03-01", "2022-04-01", "2022-05-01",
#>     "2022-06-01", "2022-07-01", "2022-08-01", "2022-09-01", "2022-10-01",
#>     "2022-11-01", "2022-12-01"
#>   ))
#> )

# we don't have to deparse everything
construct(foo, data = list(month.abb = month.abb)) # or dplyr::lst(month.abb)
#> data.frame(
#>   a = factor(month.abb),
#>   b = as.Date(c(
#>     "2022-01-01", "2022-02-01", "2022-03-01", "2022-04-01", "2022-05-01",
#>     "2022-06-01", "2022-07-01", "2022-08-01", "2022-09-01", "2022-10-01",
#>     "2022-11-01", "2022-12-01"
#>   ))
#> )

# we'll often use `dplyr::lst` and we can even use some complex expressions there
construct(list(c("a", "b", "c"), c("d", "e", "f")), data = dplyr::lst(head(letters, 3)))
#> list(head(letters, 3), c("d", "e", "f"))

# we can handle some complex attributes
library(dplyr, warn = F)
grouped_band_members <- group_by(band_members, band)
construct(grouped_band_members)
#> tibble::tibble(name = c("Mick", "John", "Paul"), band = c("Stones", "Beatles", "Beatles")) |>
#>   dplyr::group_by(band)

# And we can implement alternative constructors
construct(grouped_band_members, tribble = TRUE)
#> tibble::tribble(
#>   ~name,  ~band,
#>   "Mick", "Stones",
#>   "John", "Beatles",
#>   "Paul", "Beatles",
#> ) |>
#>   dplyr::group_by(band)

# the data arg can be a package name too
construct(grouped_band_members, data = "dplyr")
#> band_members |>
#>   dplyr::group_by(band)

# dms are supported too
library(dm, warn = F)
construct(dm(cars = head(cars)))
#> dm::dm(cars = data.frame(speed = c(4, 4, 7, 7, 8, 9), dist = c(2, 10, 4, 22, 16, 10)))

construct(dm_pixarfilms(), data = "pixarfilms")
#> dm::dm(pixar_films, pixar_people, academy, box_office, genres, public_response) |>
#>   dm::dm_add_pk(pixar_films, "film") |>
#>   dm::dm_add_pk(academy, c("film", "award_type")) |>
#>   dm::dm_add_pk(box_office, "film") |>
#>   dm::dm_add_pk(genres, c("film", "genre")) |>
#>   dm::dm_add_pk(public_response, "film") |>
#>   dm::dm_add_fk(pixar_people, "film", pixar_films, "film") |>
#>   dm::dm_add_fk(academy, "film", pixar_films, "film") |>
#>   dm::dm_add_fk(box_office, "film", pixar_films, "film") |>
#>   dm::dm_add_fk(genres, "film", pixar_films, "film") |>
#>   dm::dm_add_fk(public_response, "film", pixar_films, "film") |>
#>   dm::dm_set_colors(
#>     `#5B9BD5FF` = "pixar_films",
#>     `#70AD47FF` = "pixar_people",
#>     `#ED7D31FF` = "academy",
#>     `#ED7D31FF` = "box_office",
#>     `#ED7D31FF` = "genres",
#>     `#ED7D31FF` = "public_response"
#>   )

# environments are not always possible to reproduce but we support some common cases
search()
#>  [1] ".GlobalEnv"           "package:dm"           "package:dplyr"       
#>  [4] "package:constructive" "package:stats"        "package:graphics"    
#>  [7] "package:grDevices"    "package:utils"        "package:datasets"    
#> [10] "package:methods"      "dm_cache"             "Autoloads"           
#> [13] "package:base"
construct(as.environment(search()[1]))
#> .GlobalEnv
construct(as.environment(search()[2]))
#> as.environment("package:dm")
construct(as.environment(search()[12]))
#> as.environment("Autoloads")
construct(as.environment(search()[13]))
#> baseenv()
construct(environment(group_by))
#> asNamespace("dplyr")

# and if not possible, we try something that might sometimes be enough
e <- new.env()
e$x <- 1
e$y <- 2
construct(e)
#> as.environment(list(x = 1, y = 2))
#> Error in `construct()`:
#> ! {constructive} couldn't create code that reproduces perfectly the output
#> `original` is <env:0x157fcb990>
#> `recreated` is <env:0x121ab5828>
#> ℹ use `check = FALSE` to ignore this error

# We can construct some functions faithfully, using `rlang::new_function()`
construct(group_by)
#> Error in `construct()`:
#> ! {constructive} could not build the requested code.
#> Caused by error in `construct_idiomatic.environment()`:
#> ! formal argument "env_as_list" matched by multiple actual arguments
```

# How it works

{constructive} has a main exported function `construct()` built around a
`construct_raw()`, the main unexported function, and adding some checks
and pretty printing using {styler}.

`construct_raw` is called recursively through list-like objects and is
made of 3 steps :

-   Check if we already have the object in store in our `data` arg, if
    so display its name rather than the code to rebuild it
-   If the object cannot be found in `data` build it idiomatically by
    calling the `construct_idiomatic()` generic
-   Then the `repair_attributes()` generic adapts the above to make sure
    created object has the same class and other attributes as the source
    object

To extend the package to a new object we only need to add a method for
`construct_idiomatic()` and for `repair_attributes()`.

# Use cases

-   create reproducible examples
-   compact snapshot tests that contain all the information
-   debugging (avoiding the caveats of the default printing, where 2
    objects might print the same but not be the same)
-   Understand object structure better than when using `dput` or `str`,
    even if {constructive} doesn’t feature a specific constructor for
    the class.

``` r
x <- table(c(1, 1, 1, 4))

x
#> 
#> 1 4 
#> 3 1

construct(x)
#> c(3L, 1L) |>
#>   structure(
#>     dim = 2L,
#>     dimnames = list(c("1", "4")) |>
#>       structure(names = ""),
#>     class = "table"
#>   )

# compare with `str`, a generic which doesn't display everything:
str(x)
#>  'table' int [1:2(1d)] 3 1
#>  - attr(*, "dimnames")=List of 1
#>   ..$ : chr [1:2] "1" "4"

# and `dput()` which is often confusing, and sometimes uses different attribute names
dput(x)
#> structure(c(`1` = 3L, `4` = 1L), .Dim = 2L, .Dimnames = structure(list(
#>     c("1", "4")), .Names = ""), class = "table")

# we can use `max_atomic` to hide all atomic vectors and see only the skeleton:
construct(table(c(1,1,1,4)), max_atomic = 0)
#> `*` |>
#>   structure(
#>     dim = `*`,
#>     dimnames = list(`*`) |>
#>       structure(names = `*`),
#>     class = `*`
#>   )

construct(iris, max_atomic = 2)
#> data.frame(
#>   Sepal.Length = c(5.1, 4.9, ...),
#>   Sepal.Width = c(3.5, 3, ...),
#>   Petal.Length = c(1.4, 1.4, ...),
#>   Petal.Width = c(0.2, 0.2, ...),
#>   Species = factor(c("setosa", "setosa", ...))
#> )
```

## construct_diff

An alternative to `waldo::compare()` (looks better in the IDE without
`interactive = FALSE`)

``` r
# The args max_atomic, max_boy and env_as_list can be used to reduce output,
# here we want to see what adding geom_point() does to a ggplot
library(ggplot2)
construct_diff(
  # max_atomic limits number of displayed elements for atomic vectors
  max_atomic = 4, 
  # if env_as_list is FALSE instead of defining unnamed envs as 
  # `as.environment(as.list(elt = ...))` we simply use `new.env()`
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
