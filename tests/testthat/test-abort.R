test_that("abort", {
  expect_snapshot({
    foo <- function(x = c("a", "b"), y, z, ...) {
      .cstr_combine_errors(
        x <- rlang::arg_match(x),
        abort_not_boolean(y),
        abort_not_null_or_integerish(z),
        rlang::check_dots_empty()
      )
    }
    err <- try(foo("z","z","z","z"), silent = TRUE)
    cat(attr(err, "condition")$message)
    fun <- function(x) x
    try(abort_not_string(fun))
    try(abort_not_env_or_named_list(letters))
    try(abort_not_env_or_named_list(list(1,2)))
    try(abort_wrong_data(letters))
    try(abort_wrong_data(list(letters)))
    try(abort_wrong_data("unknown"))
    try(abort_wrong_data(list("unknown")))
    try(abort_wrong_data(fun))
    try(abort_wrong_data(list(fun)))
  })
})

test_that("describe", {
  expect_snapshot({
    fun <- function(x) x
    writeLines(describe(letters))
    writeLines(describe(fun))
  })
})
