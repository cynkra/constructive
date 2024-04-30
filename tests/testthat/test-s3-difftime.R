test_that("difftime", {
  expect_pipe_snapshot({
    construct(as.difftime(1, units = "secs"))
    construct(as.difftime(2, units = "mins"))
  })
})

