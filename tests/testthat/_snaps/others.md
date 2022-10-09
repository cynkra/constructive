# The data arg works

    Code
      construct(list(letters), data = "base")
    Output
      list(letters)
    Code
      construct(list(iris), data = "datasets")
    Output
      list(iris)
    Code
      construct(list(letters), data = baseenv())
    Output
      list(letters)
    Code
      construct(list(letters), data = list(foo = letters))
    Output
      list(foo)

# noquote is supported

    Code
      construct(noquote("a"))
    Output
      noquote("a")
    Code
      construct(noquote(list("a", "b")))
    Output
      noquote(list("a", "b"))

