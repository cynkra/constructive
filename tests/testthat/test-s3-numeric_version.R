test_that("numeric_version(), package_version(), R_system_version()", {
  expect_pipe_snapshot({
    construct(numeric_version("1.2.3"))
    construct(numeric_version("1.2.3"), opts_numeric_version("next"))
    construct(numeric_version("1.2.3"), opts_numeric_version("atomic"))
    construct(package_version("1.2.3"))
    construct(package_version("1.2.3"), opts_package_version("next"))
    construct(package_version("1.2.3"), opts_package_version("atomic"))
    construct(R_system_version("1.2.3"))
    construct(R_system_version("1.2.3"), opts_R_system_version("next"))
    construct(R_system_version("1.2.3"), opts_R_system_version("atomic"))
  })
})
