# simpleError

    Code
      construct(simpleError("hello"))
    Output
      simpleError("hello")
    Code
      construct(simpleError("hello", call = quote(a())))
    Output
      simpleError("hello", call = quote(a()))

