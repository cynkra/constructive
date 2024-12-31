test_that("tweaks work as expected", {
  expect_true(names(formals(tweaks))[[1]] == "...")
  expect_warning(tweaks(`_oooops` = 42, `_darn` = -1), "_oooops, _darn")
  expect_warning(tweaks(), NA)
  expect_warning(tweaks(5), "named")
  expect_warning(tweaks(5, `_ooops` = 42), "named")
  expect_warning(tweaks(constructor_name = "constr"), NA)
})
