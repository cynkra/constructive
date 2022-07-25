# function

    Code
      construct(as.function(alist(x = , x), .GlobalEnv))
    Output
      as.function(alist(x = , x), envir = .GlobalEnv)
    Code
      construct(identity)
    Output
      as.function(alist(x = , x), envir = .BaseNamespaceEnv)
    Code
      construct(setNames)
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
      ) |>
        rlang::zap_srcref()
    Code
      construct(setNames, max_body = 0)
    Output
      as.function(
        alist(
          object = nm,
          nm = ,
          {
            ...
          }
        ),
        envir = asNamespace("stats")
      ) |>
        rlang::zap_srcref()

