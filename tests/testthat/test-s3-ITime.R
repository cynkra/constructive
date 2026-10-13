test_that("ITime", {
  skip_if_not_installed("data.table")
  expect_snapshot({
    construct(data.table::as.ITime("12:00:00"))
    construct(stats::setNames(data.table::as.ITime(c("00:00:00", NA, "23:59:59")), c("a", "b", "c")))
    construct(data.table::as.ITime(NA))
    construct(data.table::as.ITime(character()))
    construct(structure(c(-5L, 90000L), class = "ITime"))
    construct(data.table::as.ITime("12:00:00"), opts_ITime("next"))
    construct(data.table::as.ITime("12:00:00"), opts_ITime("integer"))
  })
})
