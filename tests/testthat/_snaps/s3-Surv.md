# Surv

    Code
      x <- survival::Surv(c(1, 2, NA), c(1, 0, 1))
      construct(x)
    Output
      survival::Surv(c(1, 2, NA), c(1, 0, 1))
    Code
      construct(x, opts_Surv("next"))
    Output
      matrix(
        c(1, 2, NA, 1, 0, 1),
        nrow = 3L,
        ncol = 2L,
        dimnames = list(NULL, c("time", "status"))
      ) |>
        structure(type = "right", class = "Surv")
    Code
      construct(survival::Surv(c(1, 2), c(TRUE, FALSE)))
    Output
      survival::Surv(c(1, 2), c(1, 0))
    Code
      construct(survival::Surv(c(1, 2), c(NA, NA)))
    Output
      survival::Surv(c(1, 2), c(NA, NA))
    Code
      construct(survival::Surv(numeric(0), logical(0)))
    Output
      survival::Surv(numeric(0), logical(0))
    Code
      construct(survival::Surv(c(1, 2), c(1, 0), type = "left"))
    Output
      survival::Surv(c(1, 2), c(1, 0), type = "left")
    Code
      construct(survival::Surv(c(1, 2), c(2, 3), c(1, 0)))
    Output
      survival::Surv(c(1, 2), c(2, 3), c(1, 0))
    Code
      construct(survival::Surv(c(1, NA, 3, 4), c(2, 3, NA, 4), type = "interval2"))
    Output
      survival::Surv(c(1, NA, 3, 4), c(2, 3, NA, 4), type = "interval2")
    Code
      construct(survival::Surv(c(1, 2, 3), c(2, 3, 4), c(0, 1, 3), type = "interval"))
    Output
      survival::Surv(c(1, 2, 3), c(NA, 2, 4), type = "interval2")
    Code
      construct(survival::Surv(c(1, NA), c(2, NA), type = "interval2"))
    Output
      survival::Surv(c(1, NA), c(2, 1), c(3, NA), type = "interval")
    Code
      construct(survival::Surv(c(1, 2, 3), factor(c("a", "b", "censor"), levels = c(
        "censor", "a", "b"))))
    Output
      survival::Surv(c(1, 2, 3), factor(c("a", "b", "censor"), levels = c("censor", "a", "b")))
    Code
      construct(survival::Surv(c(0, 1, 2), c(1, 2, 3), factor(c("a", NA, "none"),
      levels = c("none", "a", "b"))))
    Output
      survival::Surv(c(0, 1, 2), c(1, 2, 3), factor(c("a", NA, "none"), levels = c("none", "a", "b")))
    Code
      x <- survival::Surv(c(1, 2, 3), factor(c("1", "2", "0")))
      attr(x, "inputAttributes") <- NULL
      construct(x)
    Output
      survival::Surv(c(1, 2, 3), factor(c("1", "2", "censor"), levels = c("censor", "1", "2"))) |>
        structure(inputAttributes = NULL)
    Code
      construct(survival::Surv(c(a = 1, b = 2), c(1, 0)))
    Output
      survival::Surv(c(1, 2), c(1, 0)) |>
        structure(inputAttributes = list(time = list(names = c("a", "b"))))
    Code
      x <- survival::Surv(c(1, 2), c(1, 0))
      rownames(x) <- c("a", "b")
      attr(x, "foo") <- "bar"
      construct(x)
    Output
      survival::Surv(c(1, 2), c(1, 0)) |>
        structure(dimnames = list(c("a", "b"), c("time", "status")), foo = "bar")

