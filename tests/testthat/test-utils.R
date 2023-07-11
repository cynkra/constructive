test_that("pipe works for one liners", {
  expect_pipe_snapshot({
    x <- 1
    attr(x, "foo") <- 2
    construct(x, one_liner = TRUE)
  })
})

test_that("data", {
  expect_snapshot({
    construct(cars, data = "datasets")
    construct(mean, data = "base")
    construct(mean, data = asNamespace("base"))
    construct(list(mean, cars), data = list(asNamespace("base"), "datasets"))
  })
})

test_that("split_by_line()", {
  expect_equal(split_by_line(""), "")
  expect_equal(split_by_line("a"), "a")
  expect_equal(split_by_line("a\n"), c("a", ""))
  expect_equal(split_by_line("a\nb"), c("a", "b"))
  expect_equal(split_by_line("a\nb\n"), c("a", "b", ""))
  expect_equal(split_by_line(c("a", "b")), c("a", "b"))
  expect_equal(split_by_line(c("a\n", "b")), c("a", "", "b"))
  expect_equal(split_by_line(c("a", "b\n")), c("a", "b", ""))
})
