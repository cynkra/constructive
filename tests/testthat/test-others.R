test_that("The data arg works", {
  expect_snapshot({
    construct(list(letters), data = "base") # the base env is a bit different
    construct(list(iris), data = "datasets")
    construct(list(letters), data = baseenv())
    construct(list(letters), data = list(foo = letters))
  })
})

test_that("noquote is supported", {
  expect_snapshot({
    construct(noquote("a"))
    construct(noquote(list("a", "b")))
  })
})
