test_that("Encoding", {
  expect_snapshot(
    construct(data.frame(
      x = c("ü","a"),
      y = c("long_enough_for_multiline_output")
    ))
  )
})
