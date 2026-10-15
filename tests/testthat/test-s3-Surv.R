test_that("Surv", {
  skip_if_not_installed("survival")
  expect_snapshot({
    # right censored
    x <- survival::Surv(c(1, 2, NA), c(1, 0, 1))
    construct(x)
    construct(x, opts_Surv("next"))
    construct(survival::Surv(c(1, 2), c(TRUE, FALSE)))
    construct(survival::Surv(c(1, 2), c(NA, NA)))
    construct(survival::Surv(numeric(0), logical(0)))
    # left censored
    construct(survival::Surv(c(1, 2), c(1, 0), type = "left"))
    # counting
    construct(survival::Surv(c(1, 2), c(2, 3), c(1, 0)))
    # interval censored
    construct(survival::Surv(c(1, NA, 3, 4), c(2, 3, NA, 4), type = "interval2"))
    construct(survival::Surv(c(1, 2, 3), c(2, 3, 4), c(0, 1, 3), type = "interval"))
    construct(survival::Surv(c(1, NA), c(2, NA), type = "interval2"))
    # multi state
    construct(survival::Surv(c(1, 2, 3), factor(c("a", "b", "censor"), levels = c("censor", "a", "b"))))
    construct(survival::Surv(c(0, 1, 2), c(1, 2, 3), factor(c("a", NA, "none"), levels = c("none", "a", "b"))))
    # attributes to repair
    construct(survival::Surv(c(a = 1, b = 2), c(1, 0)))
    x <- survival::Surv(c(1, 2), c(1, 0))
    rownames(x) <- c("a", "b")
    attr(x, "foo") <- "bar"
    construct(x)
  })
})

# Without `inputAttributes` the original factor levels cannot be recovered, so
# the generated call censors on "censor" rather than on "0". From survival
# 3.8-12 on `Surv()` records that choice in a `clabel` attribute, which we then
# have to repair; earlier versions have no such attribute.
test_that("Surv without inputAttributes, survival < 3.8-12", {
  skip_if_not_installed("survival")
  skip_if(with_versions(survival >= "3.8.12"))
  expect_snapshot({
    x <- survival::Surv(c(1, 2, 3), factor(c("1", "2", "0")))
    attr(x, "inputAttributes") <- NULL
    construct(x)
  })
})

test_that("Surv without inputAttributes, survival >= 3.8-12", {
  skip_if_not_installed("survival")
  skip_if(with_versions(survival < "3.8.12"))
  expect_snapshot({
    x <- survival::Surv(c(1, 2, 3), factor(c("1", "2", "0")))
    attr(x, "inputAttributes") <- NULL
    construct(x)
  })
})
