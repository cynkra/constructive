# ordered

    Code
      construct(factor(month.abb, rev(month.abb), ordered = TRUE))
    Output
      ordered(
        c(
          "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
          "Oct", "Nov", "Dec"
        ),
        levels = c(
          "Dec", "Nov", "Oct", "Sep", "Aug", "Jul", "Jun", "May", "Apr",
          "Mar", "Feb", "Jan"
        )
      )

