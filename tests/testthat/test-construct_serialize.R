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

test_that("construct_serialize works for numeric vectors", {
  # Simple numeric vector
  x1 <- c(1.5, 2.0, -3.14)
  code1 <- construct_serialize(x1)
  x1_reconstructed <- eval(parse(text = paste(code1, collapse = "\n")))
  expect_identical(x1_reconstructed, x1)

  # Special values
  x2 <- c(1.0, NA_real_, NaN, Inf, -Inf)
  code2 <- construct_serialize(x2)
  x2_reconstructed <- eval(parse(text = paste(code2, collapse = "\n")))
  expect_identical(x2_reconstructed, x2)

  # Empty numeric vector
  x3 <- numeric(0)
  code3 <- construct_serialize(x3)
  x3_reconstructed <- eval(parse(text = paste(code3, collapse = "\n")))
  expect_identical(x3_reconstructed, x3)

  # Very small and very large numbers
  x4 <- c(1e-300, 1e300, 0, -0)
  code4 <- construct_serialize(x4)
  x4_reconstructed <- eval(parse(text = paste(code4, collapse = "\n")))
  expect_identical(x4_reconstructed, x4)

  # All NA
  x5 <- c(NA_real_, NA_real_)
  code5 <- construct_serialize(x5)
  x5_reconstructed <- eval(parse(text = paste(code5, collapse = "\n")))
  expect_identical(x5_reconstructed, x5)

  # Single element
  x6 <- 3.14159
  code6 <- construct_serialize(x6)
  x6_reconstructed <- eval(parse(text = paste(code6, collapse = "\n")))
  expect_identical(x6_reconstructed, x6)

  # Integer-valued doubles (should maintain type)
  x7 <- c(1.0, 2.0, 3.0)
  code7 <- construct_serialize(x7)
  x7_reconstructed <- eval(parse(text = paste(code7, collapse = "\n")))
  expect_identical(x7_reconstructed, x7)
  expect_type(x7_reconstructed, "double")
})

test_that("construct_serialize handles negative zero and non-standard NaNs", {
  # Negative zero
  x1 <- -0
  code1 <- construct_serialize(x1)
  x1_reconstructed <- eval(parse(text = paste(code1, collapse = "\n")))
  expect_identical(x1_reconstructed, x1)
  # Test that it's truly negative zero
  expect_equal(1/x1_reconstructed, -Inf)

  # Vector with both zeros
  x2 <- c(0, -0)
  code2 <- construct_serialize(x2)
  x2_reconstructed <- eval(parse(text = paste(code2, collapse = "\n")))
  expect_identical(x2_reconstructed, x2)
  expect_equal(1/x2_reconstructed[1], Inf)
  expect_equal(1/x2_reconstructed[2], -Inf)

  # Non-standard NaN (if bit64 is available)
  skip_if_not_installed("bit64")
  x3 <- unclass(bit64::as.integer64(-42))
  code3 <- construct_serialize(x3)
  x3_reconstructed <- eval(parse(text = paste(code3, collapse = "\n")))
  # Both should be NaN
  expect_true(is.nan(x3))
  expect_true(is.nan(x3_reconstructed))
})

test_that("construct_serialize works for complex vectors", {
  # Simple complex vector
  x1 <- c(1+2i, 3-4i, 5+0i)
  code1 <- construct_serialize(x1)
  x1_reconstructed <- eval(parse(text = paste(code1, collapse = "\n")))
  expect_identical(x1_reconstructed, x1)

  # Special values
  x2 <- c(1+0i, NA_complex_, Inf+2i, 1+NaN*1i)
  code2 <- construct_serialize(x2)
  x2_reconstructed <- eval(parse(text = paste(code2, collapse = "\n")))
  expect_identical(x2_reconstructed, x2)

  # Empty complex vector
  x3 <- complex(0)
  code3 <- construct_serialize(x3)
  x3_reconstructed <- eval(parse(text = paste(code3, collapse = "\n")))
  expect_identical(x3_reconstructed, x3)

  # All NA
  x4 <- c(NA_complex_, NA_complex_)
  code4 <- construct_serialize(x4)
  x4_reconstructed <- eval(parse(text = paste(code4, collapse = "\n")))
  expect_identical(x4_reconstructed, x4)

  # Single element
  x5 <- 3+4i
  code5 <- construct_serialize(x5)
  x5_reconstructed <- eval(parse(text = paste(code5, collapse = "\n")))
  expect_identical(x5_reconstructed, x5)

  # Pure imaginary
  x6 <- c(0+1i, 0-1i)
  code6 <- construct_serialize(x6)
  x6_reconstructed <- eval(parse(text = paste(code6, collapse = "\n")))
  expect_identical(x6_reconstructed, x6)

  # Pure real (complex type but zero imaginary part)
  x7 <- c(5+0i, -3+0i)
  code7 <- construct_serialize(x7)
  x7_reconstructed <- eval(parse(text = paste(code7, collapse = "\n")))
  expect_identical(x7_reconstructed, x7)
  expect_type(x7_reconstructed, "complex")
})

test_that("construct_serialize works for NULL", {
  # NULL value
  x1 <- NULL
  code1 <- construct_serialize(x1)
  x1_reconstructed <- eval(parse(text = paste(code1, collapse = "\n")))
  expect_identical(x1_reconstructed, x1)
  expect_null(x1_reconstructed)
})

test_that("construct_serialize works for symbols", {
  # Simple symbol
  x1 <- as.symbol("x")
  code1 <- construct_serialize(x1)
  x1_reconstructed <- eval(parse(text = paste(code1, collapse = "\n")))
  expect_identical(x1_reconstructed, x1)
  expect_true(is.symbol(x1_reconstructed))

  # Multi-character symbol
  x2 <- as.symbol("my_variable")
  code2 <- construct_serialize(x2)
  x2_reconstructed <- eval(parse(text = paste(code2, collapse = "\n")))
  expect_identical(x2_reconstructed, x2)

  # Symbol with dots
  x3 <- as.symbol(".Internal")
  code3 <- construct_serialize(x3)
  x3_reconstructed <- eval(parse(text = paste(code3, collapse = "\n")))
  expect_identical(x3_reconstructed, x3)

  # Symbol with special characters (backticks in name)
  x4 <- as.symbol("my-var")
  code4 <- construct_serialize(x4)
  x4_reconstructed <- eval(parse(text = paste(code4, collapse = "\n")))
  expect_identical(x4_reconstructed, x4)
})

test_that("construct_serialize works for lists", {
  # Simple list with mixed types
  x1 <- list(1L, "hello", TRUE)
  code1 <- construct_serialize(x1)
  x1_reconstructed <- eval(parse(text = paste(code1, collapse = "\n")))
  expect_identical(x1_reconstructed, x1)
  expect_true(is.list(x1_reconstructed))

  # Empty list
  x2 <- list()
  code2 <- construct_serialize(x2)
  x2_reconstructed <- eval(parse(text = paste(code2, collapse = "\n")))
  expect_identical(x2_reconstructed, x2)

  # Nested list (unnamed)
  x3 <- list(1, list(2, 3))
  code3 <- construct_serialize(x3)
  x3_reconstructed <- eval(parse(text = paste(code3, collapse = "\n")))
  expect_identical(x3_reconstructed, x3)

  # List with vector elements
  x4 <- list(c(1, 2, 3), c("a", "b"), c(TRUE, FALSE))
  code4 <- construct_serialize(x4)
  x4_reconstructed <- eval(parse(text = paste(code4, collapse = "\n")))
  expect_identical(x4_reconstructed, x4)

  # Single element list
  x5 <- list(42)
  code5 <- construct_serialize(x5)
  x5_reconstructed <- eval(parse(text = paste(code5, collapse = "\n")))
  expect_identical(x5_reconstructed, x5)
})

test_that("construct_serialize works for raw vectors", {
  # Simple raw vector
  x1 <- as.raw(c(0x01, 0x02, 0xff, 0x00, 0xaa))
  code1 <- construct_serialize(x1)
  x1_reconstructed <- eval(parse(text = paste(code1, collapse = "\n")))
  expect_identical(x1_reconstructed, x1)

  # Empty raw vector
  x2 <- raw(0)
  code2 <- construct_serialize(x2)
  x2_reconstructed <- eval(parse(text = paste(code2, collapse = "\n")))
  expect_identical(x2_reconstructed, x2)

  # Single byte
  x3 <- as.raw(0x42)
  code3 <- construct_serialize(x3)
  x3_reconstructed <- eval(parse(text = paste(code3, collapse = "\n")))
  expect_identical(x3_reconstructed, x3)

  # All zeros
  x4 <- as.raw(c(0x00, 0x00, 0x00))
  code4 <- construct_serialize(x4)
  x4_reconstructed <- eval(parse(text = paste(code4, collapse = "\n")))
  expect_identical(x4_reconstructed, x4)

  # All 0xFF
  x5 <- as.raw(c(0xff, 0xff, 0xff))
  code5 <- construct_serialize(x5)
  x5_reconstructed <- eval(parse(text = paste(code5, collapse = "\n")))
  expect_identical(x5_reconstructed, x5)

  # Sequential bytes
  x6 <- as.raw(0:255)
  code6 <- construct_serialize(x6)
  x6_reconstructed <- eval(parse(text = paste(code6, collapse = "\n")))
  expect_identical(x6_reconstructed, x6)
})

test_that("construct_serialize works for pairlists", {
  # Named pairlist
  x1 <- pairlist(a = 1, b = 2)
  code1 <- construct_serialize(x1)
  x1_reconstructed <- eval(parse(text = paste(code1, collapse = "\n")))
  expect_identical(x1_reconstructed, x1)
  expect_true(is.pairlist(x1_reconstructed))

  # Unnamed pairlist
  x2 <- pairlist(1, 2, 3)
  code2 <- construct_serialize(x2)
  x2_reconstructed <- eval(parse(text = paste(code2, collapse = "\n")))
  expect_identical(x2_reconstructed, x2)

  # Empty pairlist
  x3 <- pairlist()
  code3 <- construct_serialize(x3)
  x3_reconstructed <- eval(parse(text = paste(code3, collapse = "\n")))
  expect_identical(x3_reconstructed, x3)

  # Single element named pairlist
  x4 <- pairlist(x = "hello")
  code4 <- construct_serialize(x4)
  x4_reconstructed <- eval(parse(text = paste(code4, collapse = "\n")))
  expect_identical(x4_reconstructed, x4)

  # Mixed types in pairlist
  x5 <- pairlist(a = 1L, b = "text", c = TRUE, d = 3.14)
  code5 <- construct_serialize(x5)
  x5_reconstructed <- eval(parse(text = paste(code5, collapse = "\n")))
  expect_identical(x5_reconstructed, x5)

  # Partially named pairlist
  x6 <- pairlist(a = 1, 2, c = 3)
  code6 <- construct_serialize(x6)
  x6_reconstructed <- eval(parse(text = paste(code6, collapse = "\n")))
  expect_identical(x6_reconstructed, x6)

  # Pairlist with vector elements
  x7 <- pairlist(x = c(1, 2, 3), y = c("a", "b"))
  code7 <- construct_serialize(x7)
  x7_reconstructed <- eval(parse(text = paste(code7, collapse = "\n")))
  expect_identical(x7_reconstructed, x7)
})

test_that("construct_serialize works for objects with attributes", {
  # Named numeric vector
  x1 <- c(a = 1, b = 2, c = 3)
  code1 <- construct_serialize(x1)
  x1_reconstructed <- eval(parse(text = paste(code1, collapse = "\n")))
  expect_identical(x1_reconstructed, x1)
  expect_equal(names(x1_reconstructed), c("a", "b", "c"))

  # Named character vector
  x2 <- c(first = "hello", second = "world")
  code2 <- construct_serialize(x2)
  x2_reconstructed <- eval(parse(text = paste(code2, collapse = "\n")))
  expect_identical(x2_reconstructed, x2)

  # Named list
  x3 <- list(a = 1, b = "text", c = TRUE)
  code3 <- construct_serialize(x3)
  x3_reconstructed <- eval(parse(text = paste(code3, collapse = "\n")))
  expect_identical(x3_reconstructed, x3)

  # Object with custom class attribute
  x4 <- structure(c(1, 2, 3), class = "my_class")
  code4 <- construct_serialize(x4)
  x4_reconstructed <- eval(parse(text = paste(code4, collapse = "\n")))
  expect_identical(x4_reconstructed, x4)
  expect_equal(class(x4_reconstructed), "my_class")

  # Object with multiple attributes
  x5 <- structure(1:10, names = letters[1:10], class = "custom", my_attr = "value")
  code5 <- construct_serialize(x5)
  x5_reconstructed <- eval(parse(text = paste(code5, collapse = "\n")))
  expect_identical(x5_reconstructed, x5)
})

test_that("construct_serialize works for language objects", {
  # Simple function call
  x1 <- quote(mean(x))
  code1 <- construct_serialize(x1)
  x1_reconstructed <- eval(parse(text = paste(code1, collapse = "\n")))
  expect_identical(x1_reconstructed, x1)
  expect_true(is.language(x1_reconstructed))

  # Multiple arguments
  x2 <- quote(sum(a, b, c))
  code2 <- construct_serialize(x2)
  x2_reconstructed <- eval(parse(text = paste(code2, collapse = "\n")))
  expect_identical(x2_reconstructed, x2)

  # No arguments
  x3 <- quote(foo())
  code3 <- construct_serialize(x3)
  x3_reconstructed <- eval(parse(text = paste(code3, collapse = "\n")))
  expect_identical(x3_reconstructed, x3)

  # Named arguments
  x4 <- quote(plot(x = data, y = values, main = "Title"))
  code4 <- construct_serialize(x4)
  x4_reconstructed <- eval(parse(text = paste(code4, collapse = "\n")))
  expect_identical(x4_reconstructed, x4)

  # Nested calls
  x5 <- quote(mean(log(x + 1)))
  code5 <- construct_serialize(x5)
  x5_reconstructed <- eval(parse(text = paste(code5, collapse = "\n")))
  expect_identical(x5_reconstructed, x5)

  # Binary operator
  x6 <- quote(a + b)
  code6 <- construct_serialize(x6)
  x6_reconstructed <- eval(parse(text = paste(code6, collapse = "\n")))
  expect_identical(x6_reconstructed, x6)

  # Complex expression
  x7 <- quote(if (x > 0) sqrt(x) else 0)
  code7 <- construct_serialize(x7)
  x7_reconstructed <- eval(parse(text = paste(code7, collapse = "\n")))
  expect_identical(x7_reconstructed, x7)
})
