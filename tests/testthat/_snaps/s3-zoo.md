# zoo

    Code
      construct(zoo::zoo(c(-0.06, -0.16, -1.47, -0.48, 0.42), as.Date("2003-02-01") +
        c(1, 3, 7, 9, 14) - 1))
    Output
      zoo::zoo(
        c(-0.06, -0.16, -1.47, -0.48, 0.42),
        order.by = as.Date(c("2003-02-01", "2003-02-03", "2003-02-07", "2003-02-09", "2003-02-14"))
      )
    Code
      construct(zoo::zoo(c(-0.06, -0.16, -1.47, -0.48, 0.42), c(1, 3, 7, 9, 14)))
    Output
      zoo::zoo(c(-0.06, -0.16, -1.47, -0.48, 0.42), order.by = c(1, 3, 7, 9, 14))
    Code
      construct(zoo::zoo(c(-0.06, -0.16, -1.47, -0.48, 0.42), ISOdatetime(2003, 2, c(
        1, 3, 7, 9, 14), 0, 0, 0)))
    Output
      zoo::zoo(
        c(-0.06, -0.16, -1.47, -0.48, 0.42),
        order.by = as.POSIXct(c("2003-02-01", "2003-02-03", "2003-02-07", "2003-02-09", "2003-02-14"))
      )
    Code
      mat <- matrix(1:8, nrow = 2L, ncol = 4L, dimnames = list(c("2007-01-02",
        "2007-01-03"), c("Open", "High", "Low", "Close")))
      construct(zoo::as.zoo(mat))
    Output
      zoo::zoo(
        matrix(
          1:8,
          nrow = 2L,
          ncol = 4L,
          dimnames = list(c("2007-01-02", "2007-01-03"), c("Open", "High", "Low", "Close"))
        ),
        order.by = 1:2
      )

