# function

    Code
      f1 <- as.function(alist(x = , x), .GlobalEnv)
      f2 <- as.function(alist(x = , {
        x
      }), .GlobalEnv)
      f3 <- local(function(x) {
        x
      }, .GlobalEnv)
      construct(f1)
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      function(x) x
    Code
      construct(f2)
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      function(x) {
        x
      }
    Code
      construct(f1, opts_function(environment = TRUE))
    Output
      (function(x) x) |>
        match.fun("environment<-")(.GlobalEnv)
    Code
      construct(f1, opts_function(srcref = TRUE))
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      function(x) x
    Code
      construct(f2, opts_function(srcref = TRUE))
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      function(x) {
        x
      }
    Code
      construct(f3, opts_function(srcref = TRUE))
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      (function(x) {
        x
      }) |>
        structure(
          srcref = c(5L, 13L, 7L, 1L, 13L, 1L, 5L, 7L) |>
            structure(
              srcfile = list2env(
                list(
                  wd = "/Users/Antoine/git/constructive/tests/testthat",
                  lines = c(
                    "f1 <- as.function(alist(x = , x), .GlobalEnv)", "f2 <- as.function(alist(x = , {",
                    "  x", "}), .GlobalEnv)", "f3 <- local(function(x) {", "  x",
                    "}, .GlobalEnv)", "construct(f1)", "construct(f2)", "construct(f1, opts_function(environment = TRUE))",
                    "construct(f1, opts_function(srcref = TRUE))", "construct(f2, opts_function(srcref = TRUE))",
                    "construct(f3, opts_function(srcref = TRUE))", "construct(f1, opts_function(\"as.function\"))",
                    "construct(f2, opts_function(\"as.function\"))", "construct(f1, opts_function(\"as.function\", environment = FALSE))",
                    "construct(f1, opts_function(\"new_function\"))", "construct(f2, opts_function(\"new_function\"))",
                    "construct(f1, opts_function(\"new_function\", environment = FALSE))",
                    "construct(setNames, opts_function(environment = TRUE))", "construct(setNames, opts_function(\"as.function\", environment = TRUE))",
                    "construct(setNames, opts_function(trim = 1))", "construct(`+`)",
                    "f4 <- f1", "class(f4) <- \"foo\"", "construct(f4)"
                  ),
                  Enc = "unknown",
                  isFile = FALSE,
                  timestamp = as.POSIXct("2022-10-11 20:42:23.034321") |>
                    structure(tzone = NULL),
                  filename = "<text>",
                  fixedNewlines = TRUE,
                  parseData = c(
                    1L, 1L, 1L, 2L, 1L, 263L, 1L, 3L, 1L, 4L, 1L, 5L, 1L, 266L,
                    2L, 34L, 1L, 1L, 1L, 2L, 0L, 79L, 3L, 34L, 1L, 7L, 1L, 17L, 1L,
                    296L, 4L, 6L, 1L, 18L, 1L, 18L, 1L, 40L, 5L, 32L, 1L, 7L, 1L,
                    17L, 0L, 79L, 6L, 32L, 1L, 19L, 1L, 23L, 1L, 296L, 7L, 9L, 1L,
                    24L, 1L, 24L, 1L, 40L, 8L, 21L, 1L, 19L, 1L, 23L, 0L, 79L, 9L,
                    21L, 1L, 25L, 1L, 25L, 1L, 295L, 10L, 21L, 1L, 27L, 1L, 27L,
                    1L, 294L, 11L, 21L, 1L, 29L, 1L, 29L, 1L, 44L, 12L, 21L, 1L,
                    31L, 1L, 31L, 1L, 263L, 15L, 17L, 1L, 32L, 1L, 32L, 1L, 41L,
                    16L, 21L, 1L, 31L, 1L, 31L, 0L, 79L, 17L, 21L, 1L, 19L, 1L, 32L,
                    0L, 79L, 21L, 32L, 1L, 33L, 1L, 33L, 1L, 44L, 22L, 32L, 1L, 35L,
                    1L, 44L, 1L, 263L, 26L, 28L, 1L, 45L, 1L, 45L, 1L, 41L, 27L,
                    32L, 1L, 35L, 1L, 44L, 0L, 79L, 28L, 32L, 1L, 7L, 1L, 45L, 0L,
                    79L, 32L, 34L, 1L, 1L, 1L, 45L, 0L, 79L, 34L, 0L, 2L, 1L, 2L,
                    2L, 1L, 263L, 37L, 39L, 2L, 4L, 2L, 5L, 1L, 266L, 38L, 78L, 2L,
                    1L, 2L, 2L, 0L, 79L, 39L, 78L, 2L, 7L, 2L, 17L, 1L, 296L, 40L,
                    42L, 2L, 18L, 2L, 18L, 1L, 40L, 41L, 76L, 2L, 7L, 2L, 17L, 0L,
                    79L, 42L, 76L, 2L, 19L, 2L, 23L, 1L, 296L, 43L, 45L, 2L, 24L,
                    2L, 24L, 1L, 40L, 44L, 65L, 2L, 19L, 2L, 23L, 0L, 79L, 45L, 65L,
                    2L, 25L, 2L, 25L, 1L, 295L, 46L, 65L, 2L, 27L, 2L, 27L, 1L, 294L,
                    47L, 65L, 2L, 29L, 2L, 29L, 1L, 44L, 48L, 65L, 2L, 31L, 2L, 31L,
                    1L, 123L, 51L, 60L, 3L, 3L, 3L, 3L, 1L, 263L, 53L, 55L, 3L, 3L,
                    3L, 3L, 0L, 79L, 55L, 60L, 4L, 1L, 4L, 1L, 1L, 125L, 58L, 60L,
                    2L, 31L, 4L, 1L, 0L, 79L, 60L, 65L, 4L, 2L, 4L, 2L, 1L, 41L,
                    61L, 65L, 2L, 19L, 4L, 2L, 0L, 79L, 65L, 76L, 4L, 3L, 4L, 3L,
                    1L, 44L, 66L, 76L, 4L, 5L, 4L, 14L, 1L, 263L, 70L, 72L, 4L, 15L,
                    4L, 15L, 1L, 41L, 71L, 76L, 4L, 5L, 4L, 14L, 0L, 79L, 72L, 76L,
                    2L, 7L, 4L, 15L, 0L, 79L, 76L, 78L, 2L, 1L, 4L, 15L, 0L, 79L,
                    78L, 0L, 5L, 1L, 5L, 2L, 1L, 263L, 81L, 83L, 5L, 4L, 5L, 5L,
                    1L, 266L, 82L, 116L, 5L, 1L, 5L, 2L, 0L, 79L, 83L, 116L, 5L,
                    7L, 5L, 11L, 1L, 296L, 84L, 86L, 5L, 12L, 5L, 12L, 1L, 40L, 85L,
                    114L, 5L, 7L, 5L, 11L, 0L, 79L, 86L, 114L, 5L, 13L, 5L, 20L,
                    1L, 264L, 87L, 104L, 5L, 21L, 5L, 21L, 1L, 40L, 88L, 104L, 5L,
                    22L, 5L, 22L, 1L, 292L, 89L, 104L, 5L, 23L, 5L, 23L, 1L, 41L,
                    90L, 104L, 5L, 25L, 5L, 25L, 1L, 123L, 92L, 101L, 6L, 3L, 6L,
                    3L, 1L, 263L, 94L, 96L, 6L, 3L, 6L, 3L, 0L, 79L, 96L, 101L, 7L,
                    1L, 7L, 1L, 1L, 125L, 99L, 101L, 5L, 25L, 7L, 1L, 0L, 79L, 101L,
                    104L, 7L, 2L, 7L, 2L, 1L, 44L, 102L, 114L, 5L, 13L, 7L, 1L, 0L,
                    79L, 104L, 114L, 7L, 4L, 7L, 13L, 1L, 263L, 108L, 110L, 7L, 14L,
                    7L, 14L, 1L, 41L, 109L, 114L, 7L, 4L, 7L, 13L, 0L, 79L, 110L,
                    114L, 5L, 7L, 7L, 14L, 0L, 79L, 114L, 116L, 5L, 1L, 7L, 14L,
                    0L, 79L, 116L, 0L, 8L, 1L, 8L, 9L, 1L, 296L, 119L, 121L, 8L,
                    10L, 8L, 10L, 1L, 40L, 120L, 128L, 8L, 1L, 8L, 9L, 0L, 79L, 121L,
                    128L, 8L, 11L, 8L, 12L, 1L, 263L, 122L, 124L, 8L, 13L, 8L, 13L,
                    1L, 41L, 123L, 128L, 8L, 11L, 8L, 12L, 0L, 79L, 124L, 128L, 8L,
                    1L, 8L, 13L, 0L, 79L, 128L, 0L, 9L, 1L, 9L, 9L, 1L, 296L, 132L,
                    134L, 9L, 10L, 9L, 10L, 1L, 40L, 133L, 141L, 9L, 1L, 9L, 9L,
                    0L, 79L, 134L, 141L, 9L, 11L, 9L, 12L, 1L, 263L, 135L, 137L,
                    9L, 13L, 9L, 13L, 1L, 41L, 136L, 141L, 9L, 11L, 9L, 12L, 0L,
                    79L, 137L, 141L, 9L, 1L, 9L, 13L, 0L, 79L, 141L, 0L, 10L, 1L,
                    10L, 9L, 1L, 296L, 145L, 147L, 10L, 10L, 10L, 10L, 1L, 40L, 146L,
                    170L, 10L, 1L, 10L, 9L, 0L, 79L, 147L, 170L, 10L, 11L, 10L, 12L,
                    1L, 263L, 148L, 150L, 10L, 13L, 10L, 13L, 1L, 44L, 149L, 170L,
                    10L, 11L, 10L, 12L, 0L, 79L, 150L, 170L, 10L, 15L, 10L, 27L,
                    1L, 296L, 154L, 156L, 10L, 28L, 10L, 28L, 1L, 40L, 155L, 165L,
                    10L, 15L, 10L, 27L, 0L, 79L, 156L, 165L, 10L, 29L, 10L, 39L,
                    1L, 295L, 157L, 165L, 10L, 41L, 10L, 41L, 1L, 294L, 158L, 165L,
                    10L, 43L, 10L, 46L, 1L, 261L, 159L, 160L, 10L, 43L, 10L, 46L,
                    0L, 79L, 160L, 165L, 10L, 47L, 10L, 47L, 1L, 41L, 161L, 165L,
                    10L, 15L, 10L, 47L, 0L, 79L, 165L, 170L, 10L, 48L, 10L, 48L,
                    1L, 41L, 166L, 170L, 10L, 1L, 10L, 48L, 0L, 79L, 170L, 0L, 11L,
                    1L, 11L, 9L, 1L, 296L, 174L, 176L, 11L, 10L, 11L, 10L, 1L, 40L,
                    175L, 199L, 11L, 1L, 11L, 9L, 0L, 79L, 176L, 199L, 11L, 11L,
                    11L, 12L, 1L, 263L, 177L, 179L, 11L, 13L, 11L, 13L, 1L, 44L,
                    178L, 199L, 11L, 11L, 11L, 12L, 0L, 79L, 179L, 199L, 11L, 15L,
                    11L, 27L, 1L, 296L, 183L, 185L, 11L, 28L, 11L, 28L, 1L, 40L,
                    184L, 194L, 11L, 15L, 11L, 27L, 0L, 79L, 185L, 194L, 11L, 29L,
                    11L, 34L, 1L, 295L, 186L, 194L, 11L, 36L, 11L, 36L, 1L, 294L,
                    187L, 194L, 11L, 38L, 11L, 41L, 1L, 261L, 188L, 189L, 11L, 38L,
                    11L, 41L, 0L, 79L, 189L, 194L, 11L, 42L, 11L, 42L, 1L, 41L, 190L,
                    194L, 11L, 15L, 11L, 42L, 0L, 79L, 194L, 199L, 11L, 43L, 11L,
                    43L, 1L, 41L, 195L, 199L, 11L, 1L, 11L, 43L, 0L, 79L, 199L, 0L,
                    12L, 1L, 12L, 9L, 1L, 296L, 203L, 205L, 12L, 10L, 12L, 10L, 1L,
                    40L, 204L, 228L, 12L, 1L, 12L, 9L, 0L, 79L, 205L, 228L, 12L,
                    11L, 12L, 12L, 1L, 263L, 206L, 208L, 12L, 13L, 12L, 13L, 1L,
                    44L, 207L, 228L, 12L, 11L, 12L, 12L, 0L, 79L, 208L, 228L, 12L,
                    15L, 12L, 27L, 1L, 296L, 212L, 214L, 12L, 28L, 12L, 28L, 1L,
                    40L, 213L, 223L, 12L, 15L, 12L, 27L, 0L, 79L, 214L, 223L, 12L,
                    29L, 12L, 34L, 1L, 295L, 215L, 223L, 12L, 36L, 12L, 36L, 1L,
                    294L, 216L, 223L, 12L, 38L, 12L, 41L, 1L, 261L, 217L, 218L, 12L,
                    38L, 12L, 41L, 0L, 79L, 218L, 223L, 12L, 42L, 12L, 42L, 1L, 41L,
                    219L, 223L, 12L, 15L, 12L, 42L, 0L, 79L, 223L, 228L, 12L, 43L,
                    12L, 43L, 1L, 41L, 224L, 228L, 12L, 1L, 12L, 43L, 0L, 79L, 228L,
                    0L, 13L, 1L, 13L, 9L, 1L, 296L, 232L, 234L, 13L, 10L, 13L, 10L,
                    1L, 40L, 233L, 257L, 13L, 1L, 13L, 9L, 0L, 79L, 234L, 257L, 13L,
                    11L, 13L, 12L, 1L, 263L, 235L, 237L, 13L, 13L, 13L, 13L, 1L,
                    44L, 236L, 257L, 13L, 11L, 13L, 12L, 0L, 79L, 237L, 257L, 13L,
                    15L, 13L, 27L, 1L, 296L, 241L, 243L, 13L, 28L, 13L, 28L, 1L,
                    40L, 242L, 252L, 13L, 15L, 13L, 27L, 0L, 79L, 243L, 252L, 13L,
                    29L, 13L, 34L, 1L, 295L, 244L, 252L, 13L, 36L, 13L, 36L, 1L,
                    294L, 245L, 252L, 13L, 38L, 13L, 41L, 1L, 261L, 246L, 247L, 13L,
                    38L, 13L, 41L, 0L, 79L, 247L, 252L, 13L, 42L, 13L, 42L, 1L, 41L,
                    248L, 252L, 13L, 15L, 13L, 42L, 0L, 79L, 252L, 257L, 13L, 43L,
                    13L, 43L, 1L, 41L, 253L, 257L, 13L, 1L, 13L, 43L, 0L, 79L, 257L,
                    0L, 14L, 1L, 14L, 9L, 1L, 296L, 261L, 263L, 14L, 10L, 14L, 10L,
                    1L, 40L, 262L, 284L, 14L, 1L, 14L, 9L, 0L, 79L, 263L, 284L, 14L,
                    11L, 14L, 12L, 1L, 263L, 264L, 266L, 14L, 13L, 14L, 13L, 1L,
                    44L, 265L, 284L, 14L, 11L, 14L, 12L, 0L, 79L, 266L, 284L, 14L,
                    15L, 14L, 27L, 1L, 296L, 270L, 272L, 14L, 28L, 14L, 28L, 1L,
                    40L, 271L, 279L, 14L, 15L, 14L, 27L, 0L, 79L, 272L, 279L, 14L,
                    29L, 14L, 41L, 1L, 260L, 273L, 275L, 14L, 42L, 14L, 42L, 1L,
                    41L, 274L, 279L, 14L, 29L, 14L, 41L, 0L, 79L, 275L, 279L, 14L,
                    15L, 14L, 42L, 0L, 79L, 279L, 284L, 14L, 43L, 14L, 43L, 1L, 41L,
                    280L, 284L, 14L, 1L, 14L, 43L, 0L, 79L, 284L, 0L, 15L, 1L, 15L,
                    9L, 1L, 296L, 288L, 290L, 15L, 10L, 15L, 10L, 1L, 40L, 289L,
                    311L, 15L, 1L, 15L, 9L, 0L, 79L, 290L, 311L, 15L, 11L, 15L, 12L,
                    1L, 263L, 291L, 293L, 15L, 13L, 15L, 13L, 1L, 44L, 292L, 311L,
                    15L, 11L, 15L, 12L, 0L, 79L, 293L, 311L, 15L, 15L, 15L, 27L,
                    1L, 296L, 297L, 299L, 15L, 28L, 15L, 28L, 1L, 40L, 298L, 306L,
                    15L, 15L, 15L, 27L, 0L, 79L, 299L, 306L, 15L, 29L, 15L, 41L,
                    1L, 260L, 300L, 302L, 15L, 42L, 15L, 42L, 1L, 41L, 301L, 306L,
                    15L, 29L, 15L, 41L, 0L, 79L, 302L, 306L, 15L, 15L, 15L, 42L,
                    0L, 79L, 306L, 311L, 15L, 43L, 15L, 43L, 1L, 41L, 307L, 311L,
                    15L, 1L, 15L, 43L, 0L, 79L, 311L, 0L, 16L, 1L, 16L, 9L, 1L, 296L,
                    315L, 317L, 16L, 10L, 16L, 10L, 1L, 40L, 316L, 346L, 16L, 1L,
                    16L, 9L, 0L, 79L, 317L, 346L, 16L, 11L, 16L, 12L, 1L, 263L, 318L,
                    320L, 16L, 13L, 16L, 13L, 1L, 44L, 319L, 346L, 16L, 11L, 16L,
                    12L, 0L, 79L, 320L, 346L, 16L, 15L, 16L, 27L, 1L, 296L, 324L,
                    326L, 16L, 28L, 16L, 28L, 1L, 40L, 325L, 341L, 16L, 15L, 16L,
                    27L, 0L, 79L, 326L, 341L, 16L, 29L, 16L, 41L, 1L, 260L, 327L,
                    329L, 16L, 42L, 16L, 42L, 1L, 44L, 328L, 341L, 16L, 29L, 16L,
                    41L, 0L, 79L, 329L, 341L, 16L, 44L, 16L, 54L, 1L, 295L, 333L,
                    341L, 16L, 56L, 16L, 56L, 1L, 294L, 334L, 341L, 16L, 58L, 16L,
                    62L, 1L, 261L, 335L, 336L, 16L, 58L, 16L, 62L, 0L, 79L, 336L,
                    341L, 16L, 63L, 16L, 63L, 1L, 41L, 337L, 341L, 16L, 15L, 16L,
                    63L, 0L, 79L, 341L, 346L, 16L, 64L, 16L, 64L, 1L, 41L, 342L,
                    346L, 16L, 1L, 16L, 64L, 0L, 79L, 346L, 0L, 17L, 1L, 17L, 9L,
                    1L, 296L, 350L, 352L, 17L, 10L, 17L, 10L, 1L, 40L, 351L, 373L,
                    17L, 1L, 17L, 9L, 0L, 79L, 352L, 373L, 17L, 11L, 17L, 12L, 1L,
                    263L, 353L, 355L, 17L, 13L, 17L, 13L, 1L, 44L, 354L, 373L, 17L,
                    11L, 17L, 12L, 0L, 79L, 355L, 373L, 17L, 15L, 17L, 27L, 1L, 296L,
                    359L, 361L, 17L, 28L, 17L, 28L, 1L, 40L, 360L, 368L, 17L, 15L,
                    17L, 27L, 0L, 79L, 361L, 368L, 17L, 29L, 17L, 42L, 1L, 260L,
                    362L, 364L, 17L, 43L, 17L, 43L, 1L, 41L, 363L, 368L, 17L, 29L,
                    17L, 42L, 0L, 79L, 364L, 368L, 17L, 15L, 17L, 43L, 0L, 79L, 368L,
                    373L, 17L, 44L, 17L, 44L, 1L, 41L, 369L, 373L, 17L, 1L, 17L,
                    44L, 0L, 79L, 373L, 0L, 18L, 1L, 18L, 9L, 1L, 296L, 377L, 379L,
                    18L, 10L, 18L, 10L, 1L, 40L, 378L, 400L, 18L, 1L, 18L, 9L, 0L,
                    79L, 379L, 400L, 18L, 11L, 18L, 12L, 1L, 263L, 380L, 382L, 18L,
                    13L, 18L, 13L, 1L, 44L, 381L, 400L, 18L, 11L, 18L, 12L, 0L, 79L,
                    382L, 400L, 18L, 15L, 18L, 27L, 1L, 296L, 386L, 388L, 18L, 28L,
                    18L, 28L, 1L, 40L, 387L, 395L, 18L, 15L, 18L, 27L, 0L, 79L, 388L,
                    395L, 18L, 29L, 18L, 42L, 1L, 260L, 389L, 391L, 18L, 43L, 18L,
                    43L, 1L, 41L, 390L, 395L, 18L, 29L, 18L, 42L, 0L, 79L, 391L,
                    395L, 18L, 15L, 18L, 43L, 0L, 79L, 395L, 400L, 18L, 44L, 18L,
                    44L, 1L, 41L, 396L, 400L, 18L, 1L, 18L, 44L, 0L, 79L, 400L, 0L,
                    19L, 1L, 19L, 9L, 1L, 296L, 404L, 406L, 19L, 10L, 19L, 10L, 1L,
                    40L, 405L, 435L, 19L, 1L, 19L, 9L, 0L, 79L, 406L, 435L, 19L,
                    11L, 19L, 12L, 1L, 263L, 407L, 409L, 19L, 13L, 19L, 13L, 1L,
                    44L, 408L, 435L, 19L, 11L, 19L, 12L, 0L, 79L, 409L, 435L, 19L,
                    15L, 19L, 27L, 1L, 296L, 413L, 415L, 19L, 28L, 19L, 28L, 1L,
                    40L, 414L, 430L, 19L, 15L, 19L, 27L, 0L, 79L, 415L, 430L, 19L,
                    29L, 19L, 42L, 1L, 260L, 416L, 418L, 19L, 43L, 19L, 43L, 1L,
                    44L, 417L, 430L, 19L, 29L, 19L, 42L, 0L, 79L, 418L, 430L, 19L,
                    45L, 19L, 55L, 1L, 295L, 422L, 430L, 19L, 57L, 19L, 57L, 1L,
                    294L, 423L, 430L, 19L, 59L, 19L, 63L, 1L, 261L, 424L, 425L, 19L,
                    59L, 19L, 63L, 0L, 79L, 425L, 430L, 19L, 64L, 19L, 64L, 1L, 41L,
                    426L, 430L, 19L, 15L, 19L, 64L, 0L, 79L, 430L, 435L, 19L, 65L,
                    19L, 65L, 1L, 41L, 431L, 435L, 19L, 1L, 19L, 65L, 0L, 79L, 435L,
                    0L, 20L, 1L, 20L, 9L, 1L, 296L, 439L, 441L, 20L, 10L, 20L, 10L,
                    1L, 40L, 440L, 464L, 20L, 1L, 20L, 9L, 0L, 79L, 441L, 464L, 20L,
                    11L, 20L, 18L, 1L, 263L, 442L, 444L, 20L, 19L, 20L, 19L, 1L,
                    44L, 443L, 464L, 20L, 11L, 20L, 18L, 0L, 79L, 444L, 464L, 20L,
                    21L, 20L, 33L, 1L, 296L, 448L, 450L, 20L, 34L, 20L, 34L, 1L,
                    40L, 449L, 459L, 20L, 21L, 20L, 33L, 0L, 79L, 450L, 459L, 20L,
                    35L, 20L, 45L, 1L, 295L, 451L, 459L, 20L, 47L, 20L, 47L, 1L,
                    294L, 452L, 459L, 20L, 49L, 20L, 52L, 1L, 261L, 453L, 454L, 20L,
                    49L, 20L, 52L, 0L, 79L, 454L, 459L, 20L, 53L, 20L, 53L, 1L, 41L,
                    455L, 459L, 20L, 21L, 20L, 53L, 0L, 79L, 459L, 464L, 20L, 54L,
                    20L, 54L, 1L, 41L, 460L, 464L, 20L, 1L, 20L, 54L, 0L, 79L, 464L,
                    0L, 21L, 1L, 21L, 9L, 1L, 296L, 468L, 470L, 21L, 10L, 21L, 10L,
                    1L, 40L, 469L, 499L, 21L, 1L, 21L, 9L, 0L, 79L, 470L, 499L, 21L,
                    11L, 21L, 18L, 1L, 263L, 471L, 473L, 21L, 19L, 21L, 19L, 1L,
                    44L, 472L, 499L, 21L, 11L, 21L, 18L, 0L, 79L, 473L, 499L, 21L,
                    21L, 21L, 33L, 1L, 296L, 477L, 479L, 21L, 34L, 21L, 34L, 1L,
                    40L, 478L, 494L, 21L, 21L, 21L, 33L, 0L, 79L, 479L, 494L, 21L,
                    35L, 21L, 47L, 1L, 260L, 480L, 482L, 21L, 48L, 21L, 48L, 1L,
                    44L, 481L, 494L, 21L, 35L, 21L, 47L, 0L, 79L, 482L, 494L, 21L,
                    50L, 21L, 60L, 1L, 295L, 486L, 494L, 21L, 62L, 21L, 62L, 1L,
                    294L, 487L, 494L, 21L, 64L, 21L, 67L, 1L, 261L, 488L, 489L, 21L,
                    64L, 21L, 67L, 0L, 79L, 489L, 494L, 21L, 68L, 21L, 68L, 1L, 41L,
                    490L, 494L, 21L, 21L, 21L, 68L, 0L, 79L, 494L, 499L, 21L, 69L,
                    21L, 69L, 1L, 41L, 495L, 499L, 21L, 1L, 21L, 69L, 0L, 79L, 499L,
                    0L, 22L, 1L, 22L, 9L, 1L, 296L, 503L, 505L, 22L, 10L, 22L, 10L,
                    1L, 40L, 504L, 528L, 22L, 1L, 22L, 9L, 0L, 79L, 505L, 528L, 22L,
                    11L, 22L, 18L, 1L, 263L, 506L, 508L, 22L, 19L, 22L, 19L, 1L,
                    44L, 507L, 528L, 22L, 11L, 22L, 18L, 0L, 79L, 508L, 528L, 22L,
                    21L, 22L, 33L, 1L, 296L, 512L, 514L, 22L, 34L, 22L, 34L, 1L,
                    40L, 513L, 523L, 22L, 21L, 22L, 33L, 0L, 79L, 514L, 523L, 22L,
                    35L, 22L, 38L, 1L, 295L, 515L, 523L, 22L, 40L, 22L, 40L, 1L,
                    294L, 516L, 523L, 22L, 42L, 22L, 42L, 1L, 261L, 517L, 518L, 22L,
                    42L, 22L, 42L, 0L, 79L, 518L, 523L, 22L, 43L, 22L, 43L, 1L, 41L,
                    519L, 523L, 22L, 21L, 22L, 43L, 0L, 79L, 523L, 528L, 22L, 44L,
                    22L, 44L, 1L, 41L, 524L, 528L, 22L, 1L, 22L, 44L, 0L, 79L, 528L,
                    0L, 23L, 1L, 23L, 9L, 1L, 296L, 532L, 534L, 23L, 10L, 23L, 10L,
                    1L, 40L, 533L, 541L, 23L, 1L, 23L, 9L, 0L, 79L, 534L, 541L, 23L,
                    11L, 23L, 13L, 1L, 263L, 535L, 537L, 23L, 14L, 23L, 14L, 1L,
                    41L, 536L, 541L, 23L, 11L, 23L, 13L, 0L, 79L, 537L, 541L, 23L,
                    1L, 23L, 14L, 0L, 79L, 541L, 0L, 24L, 1L, 24L, 2L, 1L, 263L,
                    545L, 547L, 24L, 4L, 24L, 5L, 1L, 266L, 546L, 551L, 24L, 1L,
                    24L, 2L, 0L, 79L, 547L, 551L, 24L, 7L, 24L, 8L, 1L, 263L, 548L,
                    550L, 24L, 7L, 24L, 8L, 0L, 79L, 550L, 551L, 24L, 1L, 24L, 8L,
                    0L, 79L, 551L, 0L, 25L, 1L, 25L, 5L, 1L, 296L, 554L, 556L, 25L,
                    6L, 25L, 6L, 1L, 40L, 555L, 563L, 25L, 1L, 25L, 5L, 0L, 79L,
                    556L, 563L, 25L, 7L, 25L, 8L, 1L, 263L, 557L, 559L, 25L, 9L,
                    25L, 9L, 1L, 41L, 558L, 563L, 25L, 7L, 25L, 8L, 0L, 79L, 559L,
                    563L, 25L, 1L, 25L, 9L, 0L, 79L, 563L, 568L, 25L, 11L, 25L, 12L,
                    1L, 266L, 564L, 568L, 25L, 14L, 25L, 18L, 1L, 260L, 565L, 567L,
                    25L, 14L, 25L, 18L, 0L, 79L, 567L, 568L, 25L, 1L, 25L, 18L, 0L,
                    79L, 568L, 0L, 26L, 1L, 26L, 9L, 1L, 296L, 571L, 573L, 26L, 10L,
                    26L, 10L, 1L, 40L, 572L, 580L, 26L, 1L, 26L, 9L, 0L, 79L, 573L,
                    580L, 26L, 11L, 26L, 12L, 1L, 263L, 574L, 576L, 26L, 13L, 26L,
                    13L, 1L, 41L, 575L, 580L, 26L, 11L, 26L, 12L, 0L, 79L, 576L,
                    580L, 26L, 1L, 26L, 13L, 0L, 79L, 580L, 0L
                  ) |>
                    structure(
                      dim = c(8L, 336L),
                      tokens = c(
                        "SYMBOL", "LEFT_ASSIGN", "expr", "SYMBOL_FUNCTION_CALL", "'('",
                        "expr", "SYMBOL_FUNCTION_CALL", "'('", "expr", "SYMBOL_SUB",
                        "EQ_SUB", "','", "SYMBOL", "')'", "expr", "expr", "','", "SYMBOL",
                        "')'", "expr", "expr", "expr", "SYMBOL", "LEFT_ASSIGN", "expr",
                        "SYMBOL_FUNCTION_CALL", "'('", "expr", "SYMBOL_FUNCTION_CALL",
                        "'('", "expr", "SYMBOL_SUB", "EQ_SUB", "','", "'{'", "SYMBOL",
                        "expr", "'}'", "expr", "')'", "expr", "','", "SYMBOL", "')'",
                        "expr", "expr", "expr", "SYMBOL", "LEFT_ASSIGN", "expr", "SYMBOL_FUNCTION_CALL",
                        "'('", "expr", "FUNCTION", "'('", "SYMBOL_FORMALS", "')'", "'{'",
                        "SYMBOL", "expr", "'}'", "expr", "','", "expr", "SYMBOL", "')'",
                        "expr", "expr", "expr", "SYMBOL_FUNCTION_CALL", "'('", "expr",
                        "SYMBOL", "')'", "expr", "expr", "SYMBOL_FUNCTION_CALL", "'('",
                        "expr", "SYMBOL", "')'", "expr", "expr", "SYMBOL_FUNCTION_CALL",
                        "'('", "expr", "SYMBOL", "','", "expr", "SYMBOL_FUNCTION_CALL",
                        "'('", "expr", "SYMBOL_SUB", "EQ_SUB", "NUM_CONST", "expr", "')'",
                        "expr", "')'", "expr", "SYMBOL_FUNCTION_CALL", "'('", "expr",
                        "SYMBOL", "','", "expr", "SYMBOL_FUNCTION_CALL", "'('", "expr",
                        "SYMBOL_SUB", "EQ_SUB", "NUM_CONST", "expr", "')'", "expr", "')'",
                        "expr", "SYMBOL_FUNCTION_CALL", "'('", "expr", "SYMBOL", "','",
                        "expr", "SYMBOL_FUNCTION_CALL", "'('", "expr", "SYMBOL_SUB",
                        "EQ_SUB", "NUM_CONST", "expr", "')'", "expr", "')'", "expr",
                        "SYMBOL_FUNCTION_CALL", "'('", "expr", "SYMBOL", "','", "expr",
                        "SYMBOL_FUNCTION_CALL", "'('", "expr", "SYMBOL_SUB", "EQ_SUB",
                        "NUM_CONST", "expr", "')'", "expr", "')'", "expr", "SYMBOL_FUNCTION_CALL",
                        "'('", "expr", "SYMBOL", "','", "expr", "SYMBOL_FUNCTION_CALL",
                        "'('", "expr", "STR_CONST", "')'", "expr", "expr", "')'", "expr",
                        "SYMBOL_FUNCTION_CALL", "'('", "expr", "SYMBOL", "','", "expr",
                        "SYMBOL_FUNCTION_CALL", "'('", "expr", "STR_CONST", "')'", "expr",
                        "expr", "')'", "expr", "SYMBOL_FUNCTION_CALL", "'('", "expr",
                        "SYMBOL", "','", "expr", "SYMBOL_FUNCTION_CALL", "'('", "expr",
                        "STR_CONST", "','", "expr", "SYMBOL_SUB", "EQ_SUB", "NUM_CONST",
                        "expr", "')'", "expr", "')'", "expr", "SYMBOL_FUNCTION_CALL",
                        "'('", "expr", "SYMBOL", "','", "expr", "SYMBOL_FUNCTION_CALL",
                        "'('", "expr", "STR_CONST", "')'", "expr", "expr", "')'", "expr",
                        "SYMBOL_FUNCTION_CALL", "'('", "expr", "SYMBOL", "','", "expr",
                        "SYMBOL_FUNCTION_CALL", "'('", "expr", "STR_CONST", "')'", "expr",
                        "expr", "')'", "expr", "SYMBOL_FUNCTION_CALL", "'('", "expr",
                        "SYMBOL", "','", "expr", "SYMBOL_FUNCTION_CALL", "'('", "expr",
                        "STR_CONST", "','", "expr", "SYMBOL_SUB", "EQ_SUB", "NUM_CONST",
                        "expr", "')'", "expr", "')'", "expr", "SYMBOL_FUNCTION_CALL",
                        "'('", "expr", "SYMBOL", "','", "expr", "SYMBOL_FUNCTION_CALL",
                        "'('", "expr", "SYMBOL_SUB", "EQ_SUB", "NUM_CONST", "expr", "')'",
                        "expr", "')'", "expr", "SYMBOL_FUNCTION_CALL", "'('", "expr",
                        "SYMBOL", "','", "expr", "SYMBOL_FUNCTION_CALL", "'('", "expr",
                        "STR_CONST", "','", "expr", "SYMBOL_SUB", "EQ_SUB", "NUM_CONST",
                        "expr", "')'", "expr", "')'", "expr", "SYMBOL_FUNCTION_CALL",
                        "'('", "expr", "SYMBOL", "','", "expr", "SYMBOL_FUNCTION_CALL",
                        "'('", "expr", "SYMBOL_SUB", "EQ_SUB", "NUM_CONST", "expr", "')'",
                        "expr", "')'", "expr", "SYMBOL_FUNCTION_CALL", "'('", "expr",
                        "SYMBOL", "')'", "expr", "expr", "SYMBOL", "LEFT_ASSIGN", "expr",
                        "SYMBOL", "expr", "expr", "SYMBOL_FUNCTION_CALL", "'('", "expr",
                        "SYMBOL", "')'", "expr", "expr", "LEFT_ASSIGN", "STR_CONST",
                        "expr", "expr", "SYMBOL_FUNCTION_CALL", "'('", "expr", "SYMBOL",
                        "')'", "expr", "expr"
                      ),
                      text = c(
                        "f1", "<-", "", "as.function", "(", "", "alist", "(", "", "x",
                        "=", ",", "x", ")", "", "", ",", ".GlobalEnv", ")", "", "", "",
                        "f2", "<-", "", "as.function", "(", "", "alist", "(", "", "x",
                        "=", ",", "{", "x", "", "}", "", ")", "", ",", ".GlobalEnv",
                        ")", "", "", "", "f3", "<-", "", "local", "(", "", "function",
                        "(", "x", ")", "{", "x", "", "}", "", ",", "", ".GlobalEnv",
                        ")", "", "", "", "construct", "(", "", "f1", ")", "", "", "construct",
                        "(", "", "f2", ")", "", "", "construct", "(", "", "f1", ",",
                        "", "opts_function", "(", "", "environment", "=", "TRUE", "",
                        ")", "", ")", "", "construct", "(", "", "f1", ",", "", "opts_function",
                        "(", "", "srcref", "=", "TRUE", "", ")", "", ")", "", "construct",
                        "(", "", "f2", ",", "", "opts_function", "(", "", "srcref", "=",
                        "TRUE", "", ")", "", ")", "", "construct", "(", "", "f3", ",",
                        "", "opts_function", "(", "", "srcref", "=", "TRUE", "", ")",
                        "", ")", "", "construct", "(", "", "f1", ",", "", "opts_function",
                        "(", "", "\"as.function\"", ")", "", "", ")", "", "construct",
                        "(", "", "f2", ",", "", "opts_function", "(", "", "\"as.function\"",
                        ")", "", "", ")", "", "construct", "(", "", "f1", ",", "", "opts_function",
                        "(", "", "\"as.function\"", ",", "", "environment", "=", "FALSE",
                        "", ")", "", ")", "", "construct", "(", "", "f1", ",", "", "opts_function",
                        "(", "", "\"new_function\"", ")", "", "", ")", "", "construct",
                        "(", "", "f2", ",", "", "opts_function", "(", "", "\"new_function\"",
                        ")", "", "", ")", "", "construct", "(", "", "f1", ",", "", "opts_function",
                        "(", "", "\"new_function\"", ",", "", "environment", "=", "FALSE",
                        "", ")", "", ")", "", "construct", "(", "", "setNames", ",",
                        "", "opts_function", "(", "", "environment", "=", "TRUE", "",
                        ")", "", ")", "", "construct", "(", "", "setNames", ",", "",
                        "opts_function", "(", "", "\"as.function\"", ",", "", "environment",
                        "=", "TRUE", "", ")", "", ")", "", "construct", "(", "", "setNames",
                        ",", "", "opts_function", "(", "", "trim", "=", "1", "", ")",
                        "", ")", "", "construct", "(", "", "`+`", ")", "", "", "f4",
                        "<-", "", "f1", "", "", "class", "(", "", "f4", ")", "", "",
                        "<-", "\"foo\"", "", "", "construct", "(", "", "f4", ")", "",
                        ""
                      ),
                      class = "parseData"
                    )
                ),
                parent = .GlobalEnv
              ) |>
                structure(class = c("srcfilecopy", "srcfile")),
              class = "srcref"
            )
        )
    Code
      construct(f1, opts_function("as.function"))
    Output
      as.function(alist(x = , x), envir = .GlobalEnv)
    Code
      construct(f2, opts_function("as.function"))
    Output
      as.function(
        alist(
          x = ,
          {
            x
          }
        ),
        envir = .GlobalEnv
      )
    Code
      construct(f1, opts_function("as.function", environment = FALSE))
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      as.function(alist(x = , x))
    Code
      construct(f1, opts_function("new_function"))
    Output
      rlang::new_function(args = alist(x = ), body = quote(x), env = .GlobalEnv)
    Code
      construct(f2, opts_function("new_function"))
    Output
      rlang::new_function(
        args = alist(x = ),
        body = quote({
          x
        }),
        env = .GlobalEnv
      )
    Code
      construct(f1, opts_function("new_function", environment = FALSE))
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      rlang::new_function(args = alist(x = ), body = quote(x))
    Code
      construct(setNames, opts_function(environment = TRUE))
    Output
      (function(object = nm, nm) {
        names(object) <- nm
        object
      }) |>
        match.fun("environment<-")(asNamespace("stats"))
    Code
      construct(setNames, opts_function("as.function", environment = TRUE))
    Output
      as.function(
        alist(
          object = nm,
          nm = ,
          {
            names(object) <- nm
            object
          }
        ),
        envir = asNamespace("stats")
      )
    Code
      construct(setNames, opts_function(trim = 1))
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      function(object = nm, nm) {
        names(object) <- nm
        ...
      }
    Code
      construct(`+`)
    Output
      .Primitive("+")
    Code
      f4 <- f1
      class(f4) <- "foo"
      construct(f4)
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      (function(x) x) |>
        structure(class = "foo")

