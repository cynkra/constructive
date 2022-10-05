test_that("formula", {
  expect_snapshot({
    # formula in global env
    evalq(construct(~a), .GlobalEnv)
    # formula in unrecognized env
    construct(local(~a))
    x <- ~a
    class(x) <- "foo"
    construct(x, check = FALSE)
    y <- ~classless
    class(y) <- NULL
    construct(y, check = FALSE)
  })
  # fail when expecting identical environment
  z <- ~d
  environment(z) <- new.env()
  expect_error(construct(z, check = TRUE), "couldn't create code")
})
