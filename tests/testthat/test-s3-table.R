test_that("table", {
  expect_snapshot({
    construct(table(c("a", "b", "a")))
    construct(table(c("a", "b", NA), useNA = "always"))
    construct(table(x = c(1, 2, 2), y = c("u", "v", "v")))
    construct(Titanic)
    construct(as.table(matrix(c(1.5, 2, 3, 4), 2)))
    construct(xtabs(~ cyl + gear, mtcars))
    construct(structure(table(c("a", "b", "a")), foo = "bar"))
    # empty tables can't be built with `as.table()`
    construct(table(character()))
    construct(table(x = c(1, 2, 2), y = c("u", "v", "v")), opts_table("next"))
  })
})
