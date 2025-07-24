test_that("S7_S3_class", {
  expect_snapshot({
    # example from `?S7::new_S3_class`

    S3_Date <- S7::new_S3_class(
      "Date",
      function(.data = integer()) {
        .Date(.data)
      },
      function(self) {
        if (!is.numeric(self)) {
          "Underlying data must be numeric"
        }
      }
    )
    # avoid changing environment adress issue
    environment(S3_Date$validator) <- .GlobalEnv
    environment(S3_Date$constructor) <- .GlobalEnv
    construct(S3_Date)
  })
})
