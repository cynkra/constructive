test_that("construct_reprex", {
  fun <- function() {
    hello <- 0
    outer(100, hello + world)
  }

  outer <- function(a, b) {
    a <- a + 1
    c <- 3
    inner1(a, c)
    inner2(a, c)
    inner3(a, c)
  }

  inner1 <- function(x, y) {
    writeLines("# 1 ------------------------------------------------------\n")
    x <- x + 1
    z <- 3
    reprex <- construct_reprex()
    print(reprex)
  }

  inner2 <- function(x, y) {
    writeLines("\n# 2 ------------------------------------------------------\n")
    x <- x + 1
    z <- 3
    reprex <- construct_reprex(1)
    print(reprex)
  }

  inner3 <- function(x, y) {
    writeLines("\n# 3 ------------------------------------------------------\n")
    x <- x + 1
    z <- 3
    .z <- 33
    reprex <- construct_reprex(2, include_dotted = FALSE)
    print(reprex)
  }

  expect_snapshot({
    fun()
  })
})
