# function

    Code
      construct(as.function(alist(x = , x), .GlobalEnv), check = FALSE)
    Output
      function(x) x
    Code
      construct(as.function(alist(x = , {
        x
      }), .GlobalEnv), check = FALSE)
    Output
      function(x) {
        x
      }
    Code
      construct(as.function(alist(x = , x), .GlobalEnv), function.construct_env = TRUE)
    Output
      (function(x) x) |>
        match.fun("environment<-")(.GlobalEnv)
    Code
      construct(as.function(alist(x = , x), .GlobalEnv), function.zap_srcref = TRUE,
      check = FALSE)
    Output
      (function(x) x)
    Code
      construct(as.function(alist(x = , {
        x
      }), .GlobalEnv), function.zap_srcref = TRUE, check = FALSE)
    Output
      (function(x) {
        x
      }) |>
        rlang::zap_srcref()
    Code
      construct(as.function(alist(x = , x), .GlobalEnv), function.as.function = TRUE,
      check = FALSE)
    Output
      as.function(alist(x = , x))
    Code
      construct(as.function(alist(x = , {
        x
      }), .GlobalEnv), function.as.function = TRUE, check = FALSE)
    Output
      as.function(
        alist(
          x = ,
          {
            x
          }
        )
      )
    Code
      construct(as.function(alist(x = , x), .GlobalEnv), function.as.function = TRUE,
      function.construct_env = TRUE)
    Output
      as.function(alist(x = , x), envir = .GlobalEnv)
    Code
      construct(as.function(alist(x = , x), .GlobalEnv), function.as.function = TRUE,
      function.zap_srcref = TRUE, check = FALSE)
    Output
      as.function(alist(x = , x))
    Code
      construct(as.function(alist(x = , {
        x
      }), .GlobalEnv), function.as.function = TRUE, function.zap_srcref = TRUE,
      check = FALSE)
    Output
      as.function(
        alist(
          x = ,
          {
            x
          }
        )
      ) |>
        rlang::zap_srcref()
    Code
      construct(setNames, function.construct_env = TRUE)
    Output
      (function(object = nm, nm) {
        names(object) <- nm
        object
      }) |>
        match.fun("environment<-")(asNamespace("stats"))
    Code
      construct(setNames, function.as.function = TRUE, function.construct_env = TRUE)
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
      construct(setNames, max_body = 0, check = FALSE)
    Output
      function(object = nm, nm) {
        ...
      }

