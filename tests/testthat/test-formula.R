test_that("formula", {
  expect_snapshot({
    # formula in global env
    evalq(construct(~a), .GlobalEnv)
    # formula in unrecognized env
    construct(local(~a), check = FALSE)
    construct(local(~a), check = FALSE, env_as_list = FALSE)
    x <- ~a
    class(x) <- "foo"
    construct(x, check = FALSE, env_as_list = FALSE)
    y <- ~classless
    class(y) <- NULL
    construct(y, check = FALSE, env_as_list = FALSE)
  })
  # fail when expecting identical environment
  z <- ~d
  environment(z) <- new.env()
  #expect_s3_class(try(construct(z), silent = TRUE), "try-error")
  expect_error(construct(z), "couldn't create code")
})
