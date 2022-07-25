# formula

    Code
      evalq(construct(~a), .GlobalEnv)
    Output
      match.fun("environment<-")(~a, .GlobalEnv)
    Code
      construct(local(~a), check = FALSE)
    Output
      match.fun("environment<-")(~a, as.environment(list()))
    Code
      construct(local(~a), check = FALSE, env_as_list = FALSE)
    Output
      match.fun("environment<-")(~a, new.env())
    Code
      x <- ~a
      class(x) <- "foo"
      construct(x, check = FALSE, env_as_list = FALSE)
    Output
      match.fun("environment<-")(~a, new.env()) |>
        structure(class = "foo")
    Code
      y <- ~classless
      class(y) <- NULL
      construct(y, check = FALSE, env_as_list = FALSE)
    Output
      match.fun("environment<-")(~classless, new.env()) |>
        structure(class = NULL)

