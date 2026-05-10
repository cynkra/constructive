test_that("Encoding", {
  expect_snapshot(
    construct(data.frame(
      x = c("ü","a"),
      y = c("loooooooooooooooooooooooooooooooooong_enough_for_multiline_output")
    ))
  )
})

test_that("non UTF-8 encodings with UTF-8 system", {
  skip_if(!l10n_info()$`UTF-8`)
  expect_snapshot({
    x <- iconv("hello\U{A0}world", to = "latin1")
    construct(x)
    x <- iconv("こんにちは", to = "shift_jis")
    construct(x)
    x <- "hello\xa0world"
    Encoding(x) <- "latin1"
    construct(x)
  })
})
