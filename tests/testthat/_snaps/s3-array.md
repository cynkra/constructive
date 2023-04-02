# array

    Code
      construct(as.array(month.abb))
    Output
      array(
        c(
          "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
          "Oct", "Nov", "Dec"
        ),
        dim = 12L
      )

---

    Code
      construct(array(1:3, c(2, 4)))
    Output
      matrix(c(1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L), nrow = 2L, ncol = 4L)

