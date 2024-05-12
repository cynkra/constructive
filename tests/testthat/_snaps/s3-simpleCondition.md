# simpleCondition

    Code
      construct(simpleCondition("hello"))
    Output
      simpleCondition("hello")
    Code
      construct(simpleCondition("hello", call = quote(a())))
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      simpleCondition("hello", "call" = quote(`"a"`()))

