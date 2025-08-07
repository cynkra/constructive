test_that("ggproto", {
  expect_construct(
    ggplot2::ggproto(
      "Adder",
      add = function(self, n) {
        self$x <- self$x + n
        self$x
      },
      x = 0
    )
  )
})
