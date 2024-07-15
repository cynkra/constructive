test_that("xts", {
  skip_if(!is_installed("xts"))
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
    construct(x, opts_xts("as.xts.data.frame"))
    construct(x, opts_xts("xts"))
    construct(x, opts_xts(".xts"))
    construct(x, opts_xts("xts"), one_liner = TRUE)
    construct(x, opts_xts(".xts"), one_liner = TRUE)
    construct(x, opts_xts("next"))
    construct_dput(x)
    construct_base(x)
  })
})
