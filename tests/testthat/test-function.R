test_that("function", {
  expect_snapshot({
    construct(as.function(alist(x=, x), .GlobalEnv), check = FALSE)
    construct(as.function(alist(x=, {x}), .GlobalEnv), check = FALSE)
    construct(as.function(alist(x=, x), .GlobalEnv), function.construct_env = TRUE)
    construct(as.function(alist(x=, x), .GlobalEnv), function.zap_srcref = TRUE, check = FALSE)
    construct(as.function(alist(x=, {x}), .GlobalEnv), function.zap_srcref = TRUE, check = FALSE)

    construct(as.function(alist(x=, x), .GlobalEnv), function.as.function = TRUE, check = FALSE)
    construct(as.function(alist(x=, {x}), .GlobalEnv), function.as.function = TRUE, check = FALSE)
    construct(as.function(alist(x=, x), .GlobalEnv), function.as.function = TRUE, function.construct_env = TRUE)
    construct(as.function(alist(x=, x), .GlobalEnv), function.as.function = TRUE, function.zap_srcref = TRUE, check = FALSE)
    construct(as.function(alist(x=, {x}), .GlobalEnv), function.as.function = TRUE, function.zap_srcref = TRUE, check = FALSE)

    construct(setNames, function.construct_env = TRUE)
    construct(setNames, function.as.function = TRUE, function.construct_env = TRUE)
    # with max_body
    construct(setNames, max_body = 0, check = FALSE)
  })
})

