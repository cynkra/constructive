test_that("xts", {
  skip_if_not_installed("xts")
  # xts object convert character dates to POSIXct with local timezone, needs
  # to be stabilized for the CI tests
  withr::local_timezone("UTC")
  expect_snapshot({
    mat <- matrix(
      c(
        50.0397819115463, 50.2304961977954, 50.420955209067, 50.3734680543285,
        50.2443255196795, 50.1321122972067, 50.0355467742705, 49.9948860954217
      ),
      nrow = 2L,
      ncol = 4L,
      dimnames = list(
        c(
          "2007-01-02", "2007-01-03"
        ),
        c("Open", "High", "Low", "Close")
      )
    )
    x <- xts::as.xts(mat)
    construct(x)
    construct(x, opts_xts("as.xts.matrix"))
    construct(x, opts_xts("as.xts.data.frame"))
    construct(x, opts_xts("xts"))
    construct(x, opts_xts(".xts"))
    construct(x, opts_xts("xts"), one_liner = TRUE)
    construct(x, opts_xts(".xts"), one_liner = TRUE)
    construct(x, opts_xts("next"))
    construct_dput(x)
    construct_base(x)
    # Date index and no column names
    construct(xts::xts(1:3, as.Date("2024-01-01") + 0:2))
  })
})
