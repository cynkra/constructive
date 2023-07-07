expect_pipe_snapshot <- function(code) {
  rlang::inject(expect_snapshot({{ code }}, transform = function(x) {
    x <- gsub("%>%", "|>", x, fixed = TRUE)
    x <- gsub("= [.]([,)])", "= _\\1", x)
    x
  }))
}
