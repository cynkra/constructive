# simpleWarning

    Code
      construct(simpleWarning("hello"))
    Output
      simpleWarning("hello")
    Code
      construct(simpleWarning("hello", call = quote(a())))
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      simpleWarning("hello", "call" = quote(`"a"`()))

