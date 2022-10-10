test_that("abort", {
  expect_snapshot({
    foo <- function(x = c("a", "b"), y, z, ...) {
      combine_errors(
        x <- rlang::arg_match(x),
        abort_not_boolean(y),
        abort_not_null_or_integerish(z),
        ellipsis::check_dots_empty()
      )
    }
    err <- try(foo("z","z","z","z"), silent = TRUE)
    cat(attr(err, "condition")$message)
  })
})
