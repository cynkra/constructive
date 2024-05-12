# errorCondition

    Code
      construct(errorCondition("hello"))
    Output
      errorCondition("hello")
    Code
      construct(errorCondition("hello", call = quote(a())))
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      errorCondition("hello", "call" = quote(`"a"`()))

