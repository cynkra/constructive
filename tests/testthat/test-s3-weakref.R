test_that("weakref", {
  expect_snapshot({
    construct(
      rlang::new_weakref(.GlobalEnv)
    )

    construct(
      rlang::new_weakref(.GlobalEnv, value = 1)
    )
  })
})
