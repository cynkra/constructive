# factor

    Code
      construct(factor(month.abb))
    Output
      factor(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
    Code
      construct(factor(month.abb), opts_factor("next"))
    Output
      c(5L, 4L, 8L, 1L, 9L, 7L, 6L, 2L, 12L, 11L, 10L, 3L) |>
        structure(
          levels = c("Apr", "Aug", "Dec", "Feb", "Jan", "Jul", "Jun", "Mar", "May", "Nov", "Oct", "Sep"),
          class = "factor"
        )
    Code
      construct(factor(month.abb), opts_factor("atomic"))
    Output
      c(5L, 4L, 8L, 1L, 9L, 7L, 6L, 2L, 12L, 11L, 10L, 3L) |>
        structure(
          levels = c("Apr", "Aug", "Dec", "Feb", "Jan", "Jul", "Jun", "Mar", "May", "Nov", "Oct", "Sep"),
          class = "factor"
        )
    Code
      construct(factor(month.abb, month.abb))
    Output
      factor(
        c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
        levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
      )
    Code
      construct(factor(month.abb), opts_factor("as_factor"))
    Output
      factor(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
    Code
      construct(factor(month.abb, month.abb), opts_factor("as_factor"))
    Output
      forcats::as_factor(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
    Code
      construct(factor(month.abb), opts_factor("new_factor"))
    Output
      vctrs::new_factor(
        c(5L, 4L, 8L, 1L, 9L, 7L, 6L, 2L, 12L, 11L, 10L, 3L),
        levels = c("Apr", "Aug", "Dec", "Feb", "Jan", "Jul", "Jun", "Mar", "May", "Nov", "Oct", "Sep")
      )
    Code
      construct(factor(month.abb, month.abb), opts_factor("new_factor"))
    Output
      vctrs::new_factor(
        1:12,
        levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
      )
    Code
      construct(factor(month.abb, levels = c(month.abb, NA), exclude = NULL))
    Output
      factor(
        c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
        levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", NA),
        exclude = NULL
      )
    Code
      construct(factor(c(a = "foo")))
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      factor("foo")

