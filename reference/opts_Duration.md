# Constructive options for class 'Duration'

These options will be used on objects of class 'Duration'.

## Usage

``` r
opts_Duration(constructor = c("default", "dseconds", "duration", "next"), ...)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object, see Details
  section.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class
\<constructive_options/constructive_options_Duration\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"default"` (default): We build the object using the largest of
  [`lubridate::dyears()`](https://lubridate.tidyverse.org/reference/duration.html),
  [`lubridate::dmonths()`](https://lubridate.tidyverse.org/reference/duration.html),
  [`lubridate::dweeks()`](https://lubridate.tidyverse.org/reference/duration.html),
  [`lubridate::ddays()`](https://lubridate.tidyverse.org/reference/duration.html),
  [`lubridate::dhours()`](https://lubridate.tidyverse.org/reference/duration.html),
  [`lubridate::dminutes()`](https://lubridate.tidyverse.org/reference/duration.html)
  and
  [`lubridate::dseconds()`](https://lubridate.tidyverse.org/reference/duration.html)
  for which all the elements are whole numbers and the object is
  reproduced exactly, so an hour is built with `lubridate::dhours(1)`
  rather than `lubridate::dseconds(3600)`. Note that the years and
  months of 'lubridate' durations are fixed approximations, of 365.25
  and 30.4375 days respectively, so 31557600 seconds come out as
  `lubridate::dyears(1)`. `NA` elements don't constrain the choice, and
  we use
  [`lubridate::dseconds()`](https://lubridate.tidyverse.org/reference/duration.html)
  if all elements are `NA` or zero, or if the object is empty.

- `"dseconds"` : We build the object using
  [`lubridate::dseconds()`](https://lubridate.tidyverse.org/reference/duration.html)
  on a number of seconds.

- `"duration"` : We build the object using
  [`lubridate::duration()`](https://lubridate.tidyverse.org/reference/duration.html)
  on a number of seconds.

- `"next"` : Use the constructor for the next supported class.
