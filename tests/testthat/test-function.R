test_that("function", {
  expect_snapshot({
    # lambda function
    construct(as.function(alist(x=, x), .GlobalEnv))
    # function from recognizable namespace, no srcref when no {
    construct(identity)
    # with src_ref
    construct(setNames)
    # with max_body
    construct(setNames, max_body = 0)
  })
})

