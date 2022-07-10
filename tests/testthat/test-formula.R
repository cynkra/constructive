test_that("formula", {
  expect_snapshot({
    # formula in global env
    evalq(construct(~a), .GlobalEnv)
    # formula in unrecognized env
    construct(local(~a), check = FALSE)
    construct(local(~a), check = FALSE, env_as_list = FALSE)
  })
})
