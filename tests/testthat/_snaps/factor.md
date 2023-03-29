# factor

    Code
      construct(factor(month.abb))
    Output
      factor(c(
        "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
        "Oct", "Nov", "Dec"
      ))
    Code
      construct(factor(month.abb, month.abb))
    Output
      factor(
        c(
          "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
          "Oct", "Nov", "Dec"
        ),
        levels = c(
          "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
          "Oct", "Nov", "Dec"
        )
      )
    Code
      construct(factor(month.abb), opts_factor("as_factor"))
    Output
      factor(c(
        "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
        "Oct", "Nov", "Dec"
      ))
    Code
      construct(factor(month.abb, month.abb), opts_factor("as_factor"))
    Output
      forcats::as_factor(c(
        "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
        "Oct", "Nov", "Dec"
      ))
    Code
      construct(factor(month.abb), opts_factor("new_factor"))
    Output
      vctrs::new_factor(
        c(5L, 4L, 8L, 1L, 9L, 7L, 6L, 2L, 12L, 11L, 10L, 3L),
        levels = c(
          "Apr", "Aug", "Dec", "Feb", "Jan", "Jul", "Jun", "Mar", "May",
          "Nov", "Oct", "Sep"
        )
      )
    Code
      construct(factor(month.abb, month.abb), opts_factor("new_factor"))
    Output
      vctrs::new_factor(
        1:12,
        levels = c(
          "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
          "Oct", "Nov", "Dec"
        )
      )
    Code
      construct(factor(c(a = "foo")))
    Output
      factor(c(a = "foo"))

