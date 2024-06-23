# ordered

    Code
      construct(factor(month.abb, ordered = TRUE))
    Output
      ordered(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
    Code
      construct(factor(month.abb, ordered = TRUE))
    Output
      ordered(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
    Code
      construct(factor(month.abb, ordered = TRUE), opts_ordered("factor"))
    Output
      factor(
        c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
        ordered = TRUE
      )
    Code
      construct(factor(month.abb, ordered = TRUE), opts_ordered("new_ordered"))
    Output
      vctrs::new_ordered(
        c(5L, 4L, 8L, 1L, 9L, 7L, 6L, 2L, 12L, 11L, 10L, 3L),
        levels = c("Apr", "Aug", "Dec", "Feb", "Jan", "Jul", "Jun", "Mar", "May", "Nov", "Oct", "Sep")
      )
    Code
      construct(factor(month.abb, ordered = TRUE), opts_ordered("next"))
    Output
      factor(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) |>
        structure(class = c("ordered", "factor"))
    Code
      construct(factor(month.abb, ordered = TRUE), opts_ordered("integer"))
    Output
      c(5L, 4L, 8L, 1L, 9L, 7L, 6L, 2L, 12L, 11L, 10L, 3L) |>
        structure(
          levels = c("Apr", "Aug", "Dec", "Feb", "Jan", "Jul", "Jun", "Mar", "May", "Nov", "Oct", "Sep"),
          class = c("ordered", "factor")
        )
    Code
      construct(factor(month.abb, month.abb, ordered = TRUE))
    Output
      ordered(
        c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
        levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
      )
    Code
      construct(factor(month.abb, month.abb, ordered = TRUE), opts_ordered("factor"))
    Output
      factor(
        c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
        levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
        ordered = TRUE
      )
    Code
      construct(factor(month.abb, month.abb, ordered = TRUE), opts_ordered(
        "new_ordered"))
    Output
      vctrs::new_ordered(
        1:12,
        levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
      )
    Code
      construct(factor(month.abb, levels = c(month.abb, NA), ordered = TRUE, exclude = NULL))
    Output
      ordered(
        c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
        levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", NA),
        exclude = NULL
      )
    Code
      construct(ordered(c(a = "foo")))
    Output
      ordered(c(a = "foo"))

