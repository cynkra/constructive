# function

    Code
      f1 <- as.function(alist(x = , x), .GlobalEnv)
      f2 <- as.function(alist(x = , {
        x
      }), .GlobalEnv)
      construct(f1)
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      function(x) x
    Code
      construct(f2)
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      function(x) {
        x
      }
    Code
      construct(f1, opts_function(environment = TRUE))
    Output
      (function(x) x) |>
        match.fun("environment<-")(.GlobalEnv)
    Code
      construct(f1, opts_function(srcref = TRUE))
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      function(x) x
    Code
      construct(f2, opts_function(srcref = TRUE))
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      function(x) {
        x
      }
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
      construct(setNames, opts_function(environment = TRUE))
    Output
      (function(object = nm, nm) {
        names(object) <- nm
        object
      }) |>
        match.fun("environment<-")(asNamespace("stats"))
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
      function(object = nm, nm) {
        names(object) <- nm
        ...
      }
    Code
      construct(`+`)
    Output
      .Primitive("+")
    Code
      f4 <- f1
      class(f4) <- "foo"
      construct(f4)
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      (function(x) x) |>
        structure(class = "foo")
    Code
      f5 <- structure((function(x) {
        x
      }), srcref = structure(c(1L, 8L, 4L, 1L, 8L, 1L, 1L, 4L), srcfile = structure(
        list2env(list(fixedNewlines = TRUE, lines = c("foo <- function(x) {",
          "  # foo", "  x", "}", ""), filename = ""), parent = .GlobalEnv), class = c(
          "srcfilecopy", "srcfile")), class = "srcref"))
      construct(f5)
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      function(x) {
        # foo
        x
      }

