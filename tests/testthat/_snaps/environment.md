# environment

    Code
      construct(globalenv())
    Output
      .GlobalEnv
    Code
      construct(baseenv())
    Output
      baseenv()
    Code
      construct(as.environment("package:base"))
    Output
      baseenv()
    Code
      construct(asNamespace("base"))
    Output
      .BaseNamespaceEnv
    Code
      construct(as.environment("Autoloads"))
    Output
      as.environment("Autoloads")
    Code
      construct(environment(setNames))
    Output
      asNamespace("stats")
    Code
      construct(as.environment(head(cars, 2)), check = FALSE)
    Output
      as.environment(list(dist = c(2, 10), speed = c(4, 4)))
    Code
      construct(as.environment(head(cars, 2)), check = FALSE, env_as_list = FALSE)
    Output
      new.env()
    Code
      construct(environment(setNames), env_as_list = FALSE)
    Output
      asNamespace("stats")

