# errorCondition

    Code
      construct(errorCondition("hello"))
    Output
      errorCondition("hello")
    Code
      construct(errorCondition("hello", call = quote(a())))
    Output
      errorCondition("hello", call = quote(a()))

