
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
#>   a = factor(
#>     c(
#>       "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
#>       "Oct", "Nov", "Dec"
#>     )
#>   ),
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
#> tibble::tibble(name = c("Mick", "John", "Paul"), band = c("Stones", "Beatles", "Beatles"), ) |>
#>   dplyr::group_by(band)

# the data arg can be a package name too
construct(grouped_band_members, data = "dplyr")
#> band_members |>
#>   dplyr::group_by(band)

# dms are supported too
library(dm, warn = F)
construct(dm(cars = head(cars)))
#> dm::dm(cars = data.frame(speed = c(4, 4, 7, 7, 8, 9), dist = c(2, 10, 4, 22, 16, 10)), ) |>
#>   structure(class = "dm", version = 2L)

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
#>   ) |>
#>   structure(class = "dm", version = 2L)

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
construct(environment(group_by))
#> asNamespace("dplyr")
```

{constructive} has a single exported function `construct()` built around
a `construct_raw()`, the main unexported function, and adding some
checks and pretty printing using {styler}.

`construct_raw` is called recursively through list-like objects and is
made of 3 steps : \* Check if we already have the object in store in our
`data` arg, if so display its name rather than the code to rebuild it \*
If the object cannot be found in `data` build it idiomatically by
calling the `construct_idiomatic()` generic \* If the

, new methods can be added to extend the package.

-   Check (by default but optionally) if the created code creates an
    object identical to the original
-   Fail gracefully if the code could not be built, or parsed,
-   Fail gracefully, by default but optionally (if `check = TRUE`), if
    the code could not be evaluated or if the created code builds an
    object different from the original
-   Print prettily using {styler}

Support can be added outside of the package by implementing your own
`construct_idiomatic.your_class` and `repair_attributes.your_class`
methods.

Overriding existing methods is not yet supported.
