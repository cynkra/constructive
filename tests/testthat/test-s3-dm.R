test_that("dm", {
  expect_pipe_snapshot({
    # simple dm
    construct(dm::dm(cars1 = head(cars,2), cars2 = tail(cars,2)), check = FALSE)
    # skip because unstable
    # construct(dm::dm(cars1 = head(cars,2), cars2 = tail(cars,2)), opts_dm("next"))
    # construct(dm::dm(cars1 = head(cars,2), cars2 = tail(cars,2)), opts_dm("list"))

    # complex dm, using existing data
    construct(dm::dm_pixarfilms(), data = "pixarfilms", check = FALSE)
  })
})
