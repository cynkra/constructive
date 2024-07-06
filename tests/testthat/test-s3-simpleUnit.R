test_that("simpleUnit", {
  expect_snapshot({
    construct(grid::unit(c(1L,1L, 1L), "cm"))
    construct_base(grid::unit(c(1L,1L, 1L), "cm"))
    construct_dput(grid::unit(c(1L,1L, 1L), "cm"))
  })
})
