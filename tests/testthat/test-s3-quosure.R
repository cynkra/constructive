test_that("quosure", {
  expect_pipe_snapshot({
    construct(rlang::new_quosure(quote(x), .GlobalEnv))
    construct(rlang::new_quosure(quote(x), .GlobalEnv), opts_quosure("next"))
    construct(rlang::new_quosure(quote(x), .GlobalEnv), opts_quosure("language"))
  })
})
