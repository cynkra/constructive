test_that("quosure", {
  expect_snapshot({
    construct(rlang::new_quosure(quote(x), .GlobalEnv))
  })
})

test_that("quosure, rlang < 1.1.7", {
  skip_if(with_versions(rlang >= "1.1.7"))
  expect_snapshot({
    construct(rlang::new_quosure(quote(x), .GlobalEnv), opts_quosure("next"))
    construct(rlang::new_quosure(quote(x), .GlobalEnv), opts_quosure("language"))
  })
})

test_that("quosure, rlang >= 1.1.7", {
  # `new_quosure()` sets the ".Environment" attribute before the class
  skip_if(with_versions(rlang < "1.1.7"))
  expect_snapshot({
    construct(rlang::new_quosure(quote(x), .GlobalEnv), opts_quosure("next"))
    construct(rlang::new_quosure(quote(x), .GlobalEnv), opts_quosure("language"))
  })
})
