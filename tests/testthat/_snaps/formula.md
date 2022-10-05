# formula

    Code
      evalq(construct(~a), .GlobalEnv)
    Output
      match.fun("environment<-")(~a, .GlobalEnv)
    Code
      construct(local(~a))
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      match.fun("environment<-")(~a, new.env(parent = asNamespace("constructive")))
    Code
      x <- ~a
      class(x) <- "foo"
      construct(x, check = FALSE)
      y <- ~classless
      class(y) <- NULL
      construct(y, check = FALSE)

