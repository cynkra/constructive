# simpleMessage

    Code
      construct(simpleMessage("hello"))
    Output
      simpleMessage("hello")
    Code
      construct(simpleMessage("hello", call = quote(a())))
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      simpleMessage("hello", "call" = quote(`"a"`()))

