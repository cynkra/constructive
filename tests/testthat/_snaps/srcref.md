# srcref

    Code
      construct(attributes(x), opts_environment(predefine = TRUE))
    Output
      ..env.1.. <- new.env(parent = emptyenv()) |>
        structure(class = c("srcfilecopy", "srcfile"))
      ..env.1..$wd <- "/Users/Antoine/git/constructive/tests/testthat"
      ..env.1..$lines <- c(
        "x <- quote({", "  1+1", "  2", "})", "test_that(\"srcref\", {",
        "  expect_snapshot({", "    construct(attributes(x), opts_environment(predefine = TRUE))",
        "  })", "})"
      )
      ..env.1..$Enc <- "unknown"
      ..env.1..$isFile <- TRUE
      ..env.1..$timestamp <- as.POSIXct("2022-11-10 01:17:49.052185") |>
        structure(tzone = NULL)
      ..env.1..$filename <- "test-srcref.R"
      ..env.1..$fixedNewlines <- TRUE
      ..env.1..$parseData <- c(
        1L, 1L, 1L, 1L, 1L, 263L, 1L, 3L, 1L, 3L, 1L, 4L, 1L, 266L,
        2L, 32L, 1L, 1L, 1L, 1L, 0L, 79L, 3L, 32L, 1L, 6L, 1L, 10L, 1L,
        296L, 4L, 6L, 1L, 11L, 1L, 11L, 1L, 40L, 5L, 30L, 1L, 6L, 1L,
        10L, 0L, 79L, 6L, 30L, 1L, 12L, 1L, 12L, 1L, 123L, 7L, 25L, 2L,
        3L, 2L, 3L, 1L, 261L, 9L, 10L, 2L, 3L, 2L, 3L, 0L, 79L, 10L,
        15L, 2L, 4L, 2L, 4L, 1L, 43L, 11L, 15L, 2L, 5L, 2L, 5L, 1L, 261L,
        12L, 13L, 2L, 5L, 2L, 5L, 0L, 79L, 13L, 15L, 2L, 3L, 2L, 5L,
        0L, 79L, 15L, 25L, 3L, 3L, 3L, 3L, 1L, 261L, 18L, 19L, 3L, 3L,
        3L, 3L, 0L, 79L, 19L, 25L, 4L, 1L, 4L, 1L, 1L, 125L, 23L, 25L,
        1L, 12L, 4L, 1L, 0L, 79L, 25L, 30L, 4L, 2L, 4L, 2L, 1L, 41L,
        26L, 30L, 1L, 6L, 4L, 2L, 0L, 79L, 30L, 32L, 1L, 1L, 4L, 2L,
        0L, 79L, 32L, 0L, 5L, 1L, 5L, 9L, 1L, 296L, 35L, 37L, 5L, 10L,
        5L, 10L, 1L, 40L, 36L, 106L, 5L, 1L, 5L, 9L, 0L, 79L, 37L, 106L,
        5L, 11L, 5L, 18L, 1L, 260L, 38L, 40L, 5L, 19L, 5L, 19L, 1L, 44L,
        39L, 106L, 5L, 11L, 5L, 18L, 0L, 79L, 40L, 106L, 5L, 21L, 5L,
        21L, 1L, 123L, 44L, 101L, 6L, 3L, 6L, 17L, 1L, 296L, 46L, 48L,
        6L, 18L, 6L, 18L, 1L, 40L, 47L, 95L, 6L, 3L, 6L, 17L, 0L, 79L,
        48L, 95L, 6L, 19L, 6L, 19L, 1L, 123L, 49L, 90L, 7L, 5L, 7L, 13L,
        1L, 296L, 51L, 53L, 7L, 14L, 7L, 14L, 1L, 40L, 52L, 84L, 7L,
        5L, 7L, 13L, 0L, 79L, 53L, 84L, 7L, 15L, 7L, 24L, 1L, 296L, 54L,
        56L, 7L, 25L, 7L, 25L, 1L, 40L, 55L, 63L, 7L, 15L, 7L, 24L, 0L,
        79L, 56L, 63L, 7L, 26L, 7L, 26L, 1L, 263L, 57L, 59L, 7L, 27L,
        7L, 27L, 1L, 41L, 58L, 63L, 7L, 26L, 7L, 26L, 0L, 79L, 59L, 63L,
        7L, 15L, 7L, 27L, 0L, 79L, 63L, 84L, 7L, 28L, 7L, 28L, 1L, 44L,
        64L, 84L, 7L, 30L, 7L, 45L, 1L, 296L, 68L, 70L, 7L, 46L, 7L,
        46L, 1L, 40L, 69L, 79L, 7L, 30L, 7L, 45L, 0L, 79L, 70L, 79L,
        7L, 47L, 7L, 55L, 1L, 295L, 71L, 79L, 7L, 57L, 7L, 57L, 1L, 294L,
        72L, 79L, 7L, 59L, 7L, 62L, 1L, 261L, 73L, 74L, 7L, 59L, 7L,
        62L, 0L, 79L, 74L, 79L, 7L, 63L, 7L, 63L, 1L, 41L, 75L, 79L,
        7L, 30L, 7L, 63L, 0L, 79L, 79L, 84L, 7L, 64L, 7L, 64L, 1L, 41L,
        80L, 84L, 7L, 5L, 7L, 64L, 0L, 79L, 84L, 90L, 8L, 3L, 8L, 3L,
        1L, 125L, 88L, 90L, 6L, 19L, 8L, 3L, 0L, 79L, 90L, 95L, 8L, 4L,
        8L, 4L, 1L, 41L, 91L, 95L, 6L, 3L, 8L, 4L, 0L, 79L, 95L, 101L,
        9L, 1L, 9L, 1L, 1L, 125L, 99L, 101L, 5L, 21L, 9L, 1L, 0L, 79L,
        101L, 106L, 9L, 2L, 9L, 2L, 1L, 41L, 102L, 106L, 5L, 1L, 9L,
        2L, 0L, 79L, 106L, 0L
      ) |>
        structure(
          dim = c(8L, 61L),
          tokens = c(
            "SYMBOL", "LEFT_ASSIGN", "expr", "SYMBOL_FUNCTION_CALL", "'('",
            "expr", "'{'", "NUM_CONST", "expr", "'+'", "NUM_CONST", "expr",
            "expr", "NUM_CONST", "expr", "'}'", "expr", "')'", "expr", "expr",
            "SYMBOL_FUNCTION_CALL", "'('", "expr", "STR_CONST", "','", "expr",
            "'{'", "SYMBOL_FUNCTION_CALL", "'('", "expr", "'{'", "SYMBOL_FUNCTION_CALL",
            "'('", "expr", "SYMBOL_FUNCTION_CALL", "'('", "expr", "SYMBOL",
            "')'", "expr", "expr", "','", "SYMBOL_FUNCTION_CALL", "'('",
            "expr", "SYMBOL_SUB", "EQ_SUB", "NUM_CONST", "expr", "')'", "expr",
            "')'", "expr", "'}'", "expr", "')'", "expr", "'}'", "expr", "')'",
            "expr"
          ),
          text = c(
            "x", "<-", "", "quote", "(", "", "{", "1", "", "+", "1", "",
            "", "2", "", "}", "", ")", "", "", "test_that", "(", "", "\"srcref\"",
            ",", "", "{", "expect_snapshot", "(", "", "{", "construct", "(",
            "", "attributes", "(", "", "x", ")", "", "", ",", "opts_environment",
            "(", "", "predefine", "=", "TRUE", "", ")", "", ")", "", "}",
            "", ")", "", "}", "", ")", ""
          ),
          class = "parseData"
        )
      list(
        srcref = list(
          srcref(..env.1.., c(1L, 12L, 1L, 12L, 12L, 12L, 1L, 1L)),
          srcref(..env.1.., c(2L, 3L, 2L, 5L, 3L, 5L, 2L, 2L)),
          srcref(..env.1.., c(3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L))
        ),
        srcfile = ..env.1..,
        wholeSrcref = srcref(..env.1.., c(1L, 0L, 4L, 1L, 0L, 1L, 1L, 4L))
      )

