# simpleError

    Code
      construct(simpleError("hello"))
    Output
      simpleError("hello")
    Code
      construct(simpleError("hello", call = quote(a())))
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      simpleError("hello", "call" = quote(`"a"`()))

