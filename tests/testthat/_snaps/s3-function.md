# function

    Code
      f1 <- as.function(alist(x = , x), .GlobalEnv)
      f2 <- as.function(alist(x = , {
        x
      }), .GlobalEnv)
      construct(f1)
    Output
      (function(x) x) |>
        (`environment<-`)(.GlobalEnv)
    Code
      construct(f2)
    Output
      (function(x) {
        x
      }) |>
        (`environment<-`)(.GlobalEnv)
    Code
      construct(f1, opts_function(environment = FALSE))
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      function(x) x
    Code
      construct(f1, opts_function(srcref = TRUE, environment = FALSE))
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      (function(x) x) |>
        structure(srcref = NULL)
    Code
      construct(f2, opts_function(srcref = TRUE, environment = FALSE))
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      (function(x) {
        x
      }) |>
        structure(srcref = NULL)
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
      setNames <- (function(object = nm, nm) {
        names(object) <- nm
        object
      })
      environment(setNames) <- asNamespace("stats")
      construct(setNames, opts_function(environment = TRUE))
    Output
      (function(object = nm, nm) {
        names(object) <- nm
        object
      }) |>
        (`environment<-`)(asNamespace("stats"))
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
      (function(object = nm, nm) {
        names(object) <- nm
        ...
      }) |>
        (`environment<-`)(asNamespace("stats"))
    Code
      construct(`+`)
    Output
      .Primitive("+")
    Code
      f4 <- f1
      class(f4) <- "foo"
      construct(f4)
    Output
      (function(x) x) |>
        (`environment<-`)(.GlobalEnv) |>
        structure(class = "foo")
    Code
      f5 <- (function(x) {
        x
      }) %>% structure(srcref = c(1L, 8L, 4L, 1L, 8L, 1L, 1L, 4L) %>% structure(
        srcfile = list2env(list(fixedNewlines = TRUE, lines = c(
          "foo <- function(x) {", "  # foo", "  x", "}", ""), filename = ""), parent = .GlobalEnv) %>%
          structure(class = c("srcfilecopy", "srcfile")), class = "srcref"))
      construct(f5, opts_function(environment = FALSE), pipe = "magrittr")
    Output
      function(x) {
        # foo
        x
      }
    Code
      f6 <- (function() NULL)
      attr(f6, "srcref") <- NULL
      construct(f6, opts_function(environment = FALSE))
    Output
      function() NULL
    Code
      f7 <- f2
      body(f7) <- structure(body(f7), some_attr = "hello")
      construct(f7, opts_function(environment = FALSE))
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      as.function(
        list(
          x = quote(expr = ),
          quote({
            x
          }) |>
            structure(some_attr = "hello")
        )
      )
    Code
      construct(as.function(list(a = list(), quote(a)), envir = .GlobalEnv))
    Output
      as.function(list(a = list(), quote(a)), envir = .GlobalEnv)

