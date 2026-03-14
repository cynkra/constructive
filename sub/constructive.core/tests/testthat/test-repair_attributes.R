test_that("structure corner cases", {
  a <- 1
  attributes(a) <- list(.Names = "name1", names = "name2", .Label = "label1", levels = "label2")
  expect_snapshot(construct(a))
})
