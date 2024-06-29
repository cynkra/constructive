# self reference fails properly

    Code
      construct(env, opts_environment("predefine"))
    Output
      ..env.1.. <- new.env(parent = baseenv())
      ..env.1..$x <- list(..env.1..)
      ..env.1..

---

    Code
      construct(env, opts_environment("predefine"))
    Output
      ..env.1.. <- new.env(parent = baseenv())
      ..env.1..$x <- 1 |>
        structure(foo = ..env.1..)
      ..env.1..

---

    Code
      construct(env, opts_environment("predefine"))
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      ..env.1.. <- new.env(parent = baseenv()) |>
        structure(foo = ..env.1..)
      ..env.1..

---

    Code
      construct(env, opts_environment("predefine"))
    Output
      ..env.1.. <- new.env(parent = baseenv())
      ..env.1..$f <- (function() NULL) |>
        (`environment<-`)(..env.1..)
      ..env.1..

