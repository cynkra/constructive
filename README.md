
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

# the data arg can be a package name too
construct(grouped_band_members, data = "dplyr")
#> band_members |>
#>   dplyr::group_by(band)

# dms are supported too
library(dm, warn = F)
construct(dm(cars = head(cars)))
#> dm::dm(cars = data.frame(speed = c(4, 4, 7, 7, 8, 9), dist = c(2, 10, 4, 22, 16, 10)))

construct(dm_pixarfilms(), data = "pixarfilms")
#> dm::dm(
#>   pixar_films = pixar_films,
#>   pixar_people = pixar_people,
#>   academy = academy,
#>   box_office = box_office,
#>   genres = genres,
#>   public_response = public_response,
#> ) |>
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
#> .BaseNamespaceEnv
#> Error in `construct()`:
#> ! {constructive} couldn't create code that reproduces perfectly the output
#> `original` is <env:package:base>
#> `recreated` is <env:namespace:base>
#> ℹ use `check = FALSE` to ignore this error
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
#> `original` is <env:0x1389c0a80>
#> `recreated` is <env:0x11f33a0a8>
#> ℹ use `check = FALSE` to ignore this error
```

# How it works

{constructive} has a single exported function `construct()` built around
a `construct_raw()`, the main unexported function, and adding some
checks and pretty printing using {styler}.

`construct_raw` is called recursively through list-like objects and is
made of 3 steps : \* Check if we already have the object in store in our
`data` arg, if so display its name rather than the code to rebuild it \*
If the object cannot be found in `data` build it idiomatically by
calling the `construct_idiomatic()` generic \* Then the
`repair_attributes()` generic adapts the above to make sure created
object has the same class and other attributes as the source object

To extend the package to a new object we only need to add a method for
`construct_idiomatic()` and for `repair_attributes()`.
