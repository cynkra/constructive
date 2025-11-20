test_that("construct_serialize works for character vectors", {
  # Simple character vector
  x1 <- c("a", "b", "c")
  code1 <- construct_serialize(x1)
  x1_reconstructed <- eval(parse(text = paste(code1, collapse = "\n")))
  expect_identical(x1_reconstructed, x1)

  # Multi-byte characters (UTF-8)
  x2 <- c("ab", "aé")
  code2 <- construct_serialize(x2)
  x2_reconstructed <- eval(parse(text = paste(code2, collapse = "\n")))
  expect_identical(x2_reconstructed, x2)

  # Empty character vector
  x3 <- character(0)
  code3 <- construct_serialize(x3)
  x3_reconstructed <- eval(parse(text = paste(code3, collapse = "\n")))
  expect_identical(x3_reconstructed, x3)

  # Single element
  x4 <- "hello"
  code4 <- construct_serialize(x4)
  x4_reconstructed <- eval(parse(text = paste(code4, collapse = "\n")))
  expect_identical(x4_reconstructed, x4)

  # Character vector with NA
  x5 <- c("a", NA, "b")
  code5 <- construct_serialize(x5)
  x5_reconstructed <- eval(parse(text = paste(code5, collapse = "\n")))
  expect_identical(x5_reconstructed, x5)

  # Multiple NAs
  x6 <- c(NA, NA, "test")
  code6 <- construct_serialize(x6)
  x6_reconstructed <- eval(parse(text = paste(code6, collapse = "\n")))
  expect_identical(x6_reconstructed, x6)

  # All NAs
  x7 <- c(NA_character_, NA_character_)
  code7 <- construct_serialize(x7)
  x7_reconstructed <- eval(parse(text = paste(code7, collapse = "\n")))
  expect_identical(x7_reconstructed, x7)
})

test_that("construct_serialize returns constructive_code class", {
  x <- c("a", "b")
  code <- construct_serialize(x)
  expect_s3_class(code, "constructive_code")
  expect_type(code, "character")
})

test_that("construct_serialize output has expected structure", {
  x <- c("test")
  code <- construct_serialize(x)

  # Check that it starts with unserialize
  expect_match(code[1], "^unserialize\\(as\\.raw\\(c\\($")

  # Check for header and data sections
  expect_true(any(grepl("# --- HEADER ---", code)))
  expect_true(any(grepl("# --- DATA ---", code)))

  # Check that it ends properly
  expect_match(code[length(code)], "^\\)\\)\\)$")
})

test_that("construct_serialize works for logical vectors", {
  # Simple logical vector
  x1 <- c(TRUE, FALSE, TRUE)
  code1 <- construct_serialize(x1)
  x1_reconstructed <- eval(parse(text = paste(code1, collapse = "\n")))
  expect_identical(x1_reconstructed, x1)

  # Logical vector with NA
  x2 <- c(TRUE, NA, FALSE)
  code2 <- construct_serialize(x2)
  x2_reconstructed <- eval(parse(text = paste(code2, collapse = "\n")))
  expect_identical(x2_reconstructed, x2)

  # Empty logical vector
  x3 <- logical(0)
  code3 <- construct_serialize(x3)
  x3_reconstructed <- eval(parse(text = paste(code3, collapse = "\n")))
  expect_identical(x3_reconstructed, x3)

  # All TRUE
  x4 <- c(TRUE, TRUE, TRUE)
  code4 <- construct_serialize(x4)
  x4_reconstructed <- eval(parse(text = paste(code4, collapse = "\n")))
  expect_identical(x4_reconstructed, x4)

  # All FALSE
  x5 <- c(FALSE, FALSE)
  code5 <- construct_serialize(x5)
  x5_reconstructed <- eval(parse(text = paste(code5, collapse = "\n")))
  expect_identical(x5_reconstructed, x5)

  # All NA
  x6 <- c(NA, NA)
  code6 <- construct_serialize(x6)
  x6_reconstructed <- eval(parse(text = paste(code6, collapse = "\n")))
  expect_identical(x6_reconstructed, x6)

  # Single element
  x7 <- TRUE
  code7 <- construct_serialize(x7)
  x7_reconstructed <- eval(parse(text = paste(code7, collapse = "\n")))
  expect_identical(x7_reconstructed, x7)
})

test_that("construct_serialize works for integer vectors", {
  # Simple integer vector
  x1 <- c(1L, 2L, 3L)
  code1 <- construct_serialize(x1)
  x1_reconstructed <- eval(parse(text = paste(code1, collapse = "\n")))
  expect_identical(x1_reconstructed, x1)

  # Integer vector with NA
  x2 <- c(1L, NA_integer_, 3L)
  code2 <- construct_serialize(x2)
  x2_reconstructed <- eval(parse(text = paste(code2, collapse = "\n")))
  expect_identical(x2_reconstructed, x2)

  # Negative integers
  x3 <- c(-5L, 0L, -100L)
  code3 <- construct_serialize(x3)
  x3_reconstructed <- eval(parse(text = paste(code3, collapse = "\n")))
  expect_identical(x3_reconstructed, x3)

  # Empty integer vector
  x4 <- integer(0)
  code4 <- construct_serialize(x4)
  x4_reconstructed <- eval(parse(text = paste(code4, collapse = "\n")))
  expect_identical(x4_reconstructed, x4)

  # Large integers (boundary values)
  x5 <- c(2147483647L, -2147483647L, 1000000L)
  code5 <- construct_serialize(x5)
  x5_reconstructed <- eval(parse(text = paste(code5, collapse = "\n")))
  expect_identical(x5_reconstructed, x5)

  # All NA
  x6 <- c(NA_integer_, NA_integer_)
  code6 <- construct_serialize(x6)
  x6_reconstructed <- eval(parse(text = paste(code6, collapse = "\n")))
  expect_identical(x6_reconstructed, x6)

  # Single element
  x7 <- 42L
  code7 <- construct_serialize(x7)
  x7_reconstructed <- eval(parse(text = paste(code7, collapse = "\n")))
  expect_identical(x7_reconstructed, x7)
})
