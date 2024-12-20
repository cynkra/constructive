# numeric_version(), package_version(), R_system_version()

    Code
      construct(numeric_version("1.2.3"))
    Output
      numeric_version("1.2.3")
    Code
      construct(numeric_version(c("1.2.3", "4.5.6.7")))
    Output
      numeric_version(c("1.2.3", "4.5.6.7"))
    Code
      construct(numeric_version("1.2.3"), opts_numeric_version("next"))
    Output
      list(1:3) |>
        structure(class = "numeric_version")
    Code
      construct(numeric_version("1.2.3"), opts_numeric_version("list"))
    Output
      list(1:3) |>
        structure(class = "numeric_version")
    Code
      construct(package_version("1.2.3"))
    Output
      package_version("1.2.3")
    Code
      construct(package_version(c("1.2.3", "4.5.6.7")))
    Output
      package_version(c("1.2.3", "4.5.6.7"))
    Code
      construct(package_version("1.2.3"), opts_package_version("next"))
    Output
      numeric_version("1.2.3") |>
        structure(class = c("package_version", "numeric_version"))
    Code
      construct(package_version("1.2.3"), opts_package_version("list"))
    Output
      list(1:3) |>
        structure(class = c("package_version", "numeric_version"))
    Code
      construct(R_system_version("1.2.3"))
    Output
      R_system_version("1.2.3")
    Code
      construct(R_system_version(c("1.2.3", "4.5.6")))
    Output
      R_system_version(c("1.2.3", "4.5.6"))
    Code
      construct(R_system_version("1.2.3"), opts_R_system_version("next"))
    Output
      package_version("1.2.3") |>
        structure(class = c("R_system_version", "package_version", "numeric_version"))
    Code
      construct(R_system_version("1.2.3"), opts_R_system_version("list"))
    Output
      list(1:3) |>
        structure(class = c("R_system_version", "package_version", "numeric_version"))

