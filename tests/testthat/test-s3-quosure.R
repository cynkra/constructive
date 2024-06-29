test_that("quosure", {
  expect_snapshot({
    construct(rlang::new_quosure(quote(x), .GlobalEnv))
    construct(rlang::new_quosure(quote(x), .GlobalEnv), opts_quosure("next"))
    construct(rlang::new_quosure(quote(x), .GlobalEnv), opts_quosure("language"))
  })
})
