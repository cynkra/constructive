# ftable

    Code
      construct(ftable(table(a = c(1, 2, 2), b = c("x", "y", "y"))))
    Output
      ftable(
        as.table(
          matrix(
            c(1L, 0L, 0L, 2L),
            nrow = 2L,
            ncol = 2L,
            dimnames = list(a = c("1", "2"), b = c("x", "y"))
          )
        )
      )
    Code
      construct(ftable(as.table(matrix(1:4, 2))))
    Output
      ftable(
        as.table(matrix(1:4, nrow = 2L, ncol = 2L, dimnames = list(c("A", "B"), c("A", "B"))))
      )
    Code
      construct(ftable(Titanic, row.vars = 1:2))
    Output
      ftable(
        as.table(
          array(
            c(
              0, 0, 35, 0, 0, 0, 17, 0, 118, 154, 387, 670, 4, 13, 89, 3, 5, 11, 13, 0, 1,
              13, 14, 0, 57, 14, 75, 192, 140, 80, 76, 20
            ),
            dim = c(4L, 2L, 2L, 2L),
            dimnames = list(
              Class = c("1st", "2nd", "3rd", "Crew"),
              Sex = c("Male", "Female"),
              Age = c("Child", "Adult"),
              Survived = c("No", "Yes")
            )
          )
        ),
        row.vars = c("Class", "Sex")
      )
    Code
      construct(ftable(Titanic, row.vars = c(4, 1), col.vars = 2))
    Output
      ftable(
        as.table(
          array(
            c(118, 62, 154, 25, 422, 88, 670, 192, 4, 141, 13, 93, 106, 90, 3, 20),
            dim = c(2L, 4L, 2L),
            dimnames = list(
              Survived = c("No", "Yes"),
              Class = c("1st", "2nd", "3rd", "Crew"),
              Sex = c("Male", "Female")
            )
          )
        )
      )
    Code
      construct(ftable(table(a = c(1, 2, 2), b = c("x", "y", "y"))), opts_ftable(
        "next"))
    Output
      matrix(c(1L, 0L, 0L, 2L), nrow = 2L, ncol = 2L) |>
        structure(
          class = "ftable",
          row.vars = list(a = c("1", "2")),
          col.vars = list(b = c("x", "y"))
        )

