test_that("numeric_version(), package_version(), R_system_version()", {
  expect_snapshot({
    construct(numeric_version("1.2.3"))
    construct(package_version("1.2.3"))
    construct(R_system_version("1.2.3"))
    })
})
