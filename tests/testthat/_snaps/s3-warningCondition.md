# warningCondition

    Code
      construct(warningCondition("hello"))
    Output
      warningCondition("hello")
    Code
      construct(warningCondition("hello", call = quote(a())))
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      warningCondition("hello", "call" = quote(`"a"`()))

