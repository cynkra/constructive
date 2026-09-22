# haven_labelled

    Code
      construct(haven::labelled(c(1, 2, NA), labels = c(Male = 1, Female = 2), label = "Sex"))
    Output
      haven::labelled(c(1, 2, NA), labels = c(Male = 1, Female = 2), label = "Sex")
    Code
      construct(haven::labelled(1:3))
    Output
      haven::labelled(1:3)
    Code
      construct(haven::labelled(double()))
    Output
      haven::labelled(numeric(0))
    Code
      construct(haven::labelled(c("a", "b"), labels = c(A = "a")))
    Output
      haven::labelled(c("a", "b"), labels = c(A = "a"))
    Code
      construct(haven::labelled(c(a = 1, b = 2), labels = c(x = 1)))
    Output
      haven::labelled(c(a = 1, b = 2), labels = c(x = 1))
    Code
      construct(structure(haven::labelled(1:3, labels = c(A = 1L)), foo = "bar"))
    Output
      haven::labelled(1:3, labels = c(A = 1L)) |>
        structure(foo = "bar")
    Code
      construct(data.frame(sex = haven::labelled(c(1, 2), labels = c(Male = 1,
        Female = 2))))
    Output
      data.frame(sex = haven::labelled(c(1, 2), labels = c(Male = 1, Female = 2)))
    Code
      construct(haven::labelled(c(1, 2), labels = c(Male = 1)), opts_haven_labelled(
        "labelled"))
    Output
      haven::labelled(c(1, 2), labels = c(Male = 1))
    Code
      construct(haven::labelled(c(1, 2), labels = c(Male = 1)), opts_haven_labelled(
        "next"))
    Output
      c(1, 2) |>
        structure(labels = c(Male = 1), class = c("haven_labelled", "vctrs_vctr", "double"))
    Code
      construct(structure(TRUE, class = c("haven_labelled", "vctrs_vctr", "logical")))
    Output
      TRUE |>
        structure(class = c("haven_labelled", "vctrs_vctr", "logical"))

