test_that("constructive_options", {
  expect_snapshot({
    construct(opts_Date("as.Date"))
    construct(opts_Date("as.Date"), opts_constructive_options("next"))
    construct(opts_Date("new_date"))
    construct(opts_Date(origin = "2020-01-01"))
  })
})
