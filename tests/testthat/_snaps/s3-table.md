# table

    Code
      construct(table(c("a", "b", "a")))
    Output
      as.table(
        array(
          2:1,
          dim = 2L,
          dimnames = list(c("a", "b")) |>
            structure(names = "")
        )
      )
    Code
      construct(table(c("a", "b", NA), useNA = "always"))
    Output
      as.table(
        array(
          rep(1L, 3L),
          dim = 3L,
          dimnames = list(c("a", "b", NA)) |>
            structure(names = "")
        )
      )
    Code
      construct(table(x = c(1, 2, 2), y = c("u", "v", "v")))
    Output
      as.table(
        matrix(
          c(1L, 0L, 0L, 2L),
          nrow = 2L,
          ncol = 2L,
          dimnames = list(x = c("1", "2"), y = c("u", "v"))
        )
      )
    Code
      construct(Titanic)
    Output
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
      )
    Code
      construct(as.table(matrix(c(1.5, 2, 3, 4), 2)))
    Output
      as.table(
        matrix(c(1.5, 2, 3, 4), nrow = 2L, ncol = 2L, dimnames = list(c("A", "B"), c("A", "B")))
      )
    Code
      construct(xtabs(~ cyl + gear, mtcars))
    Output
      as.table(
        matrix(
          c(1L, 2L, 12L, 8L, 4L, 0L, 2L, 1L, 2L),
          nrow = 3L,
          ncol = 3L,
          dimnames = list(cyl = c("4", "6", "8"), gear = c("3", "4", "5"))
        )
      ) |>
        structure(
          class = c("xtabs", "table"),
          call = quote(xtabs(formula = ~cyl + gear, data = mtcars))
        )
    Code
      construct(structure(table(c("a", "b", "a")), foo = "bar"))
    Output
      as.table(
        array(
          2:1,
          dim = 2L,
          dimnames = list(c("a", "b")) |>
            structure(names = "")
        )
      ) |>
        structure(foo = "bar")
    Code
      construct(table(character()))
    Output
      array(
        integer(0),
        dim = 0L,
        dimnames = list(NULL) |>
          structure(names = "")
      ) |>
        structure(class = "table")
    Code
      construct(table(x = c(1, 2, 2), y = c("u", "v", "v")), opts_table("next"))
    Output
      matrix(
        c(1L, 0L, 0L, 2L),
        nrow = 2L,
        ncol = 2L,
        dimnames = list(x = c("1", "2"), y = c("u", "v"))
      ) |>
        structure(class = "table")

