expect_pipe_snapshot <- function(code) {
  transform <- function(out) {
    out <- gsub("%>%", "|>", out, fixed = TRUE)
    out <- gsub("= [.]([,)])", "= _\\1", out)
    out
  }
  rlang::inject(testthat::expect_snapshot({{ code }}, transform = transform))
}
