# function

    Code
      construct(as.function(alist(x = , x), .GlobalEnv))
    Output
      rlang::new_function(alist(x = ), quote(x), .GlobalEnv)
    Code
      construct(identity)
    Output
      rlang::new_function(alist(x = ), quote(x), .BaseNamespaceEnv)
    Code
      construct(setNames)
    Output
      rlang::new_function(
        alist(object = nm, nm = ),
        quote({
          names(object) <- nm
          object
        }),
        asNamespace("stats")
      ) |>
        rlang::zap_srcref()

