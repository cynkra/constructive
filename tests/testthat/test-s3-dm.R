test_that("dm", {
  expect_snapshot({
    # simple dm
    construct(dm::dm(cars1 = head(cars,2), cars2 = tail(cars,2)), check = FALSE)
    # complex dm, using existing data
    construct(dm::dm_pixarfilms(), data = "pixarfilms", check = FALSE)
  })
})
