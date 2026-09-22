# haven_labelled_spss

    Code
      construct(haven::labelled_spss(c(1, 2, -99), labels = c(Male = 1, Missing = -99),
      na_values = -99, na_range = c(90, Inf), label = "Sex"))
    Output
      haven::labelled_spss(
        c(1, 2, -99),
        labels = c(Male = 1, Missing = -99),
        na_values = -99,
        na_range = c(90, Inf),
        label = "Sex"
      )
    Code
      construct(haven::labelled_spss(1:2))
    Output
      haven::labelled_spss(1:2)
    Code
      construct(haven::labelled_spss(character()))
    Output
      haven::labelled_spss(character(0))
    Code
      construct(haven::labelled_spss(c("a", "Z", NA), labels = c(A = "a"), na_values = "Z",
      na_range = c("X", "Z")))
    Output
      haven::labelled_spss(c("a", "Z", NA), labels = c(A = "a"), na_values = "Z", na_range = c("X", "Z"))
    Code
      construct(structure(haven::labelled_spss(c(a = 1, b = -99), na_values = -99),
      foo = "bar"))
    Output
      haven::labelled_spss(c(a = 1, b = -99), na_values = -99) |>
        structure(foo = "bar")
    Code
      x <- haven::labelled_spss(c(1, -99), labels = c(Missing = -99), na_values = -99)
      construct(x, opts_haven_labelled_spss("labelled_spss"))
    Output
      haven::labelled_spss(c(1, -99), labels = c(Missing = -99), na_values = -99)
    Code
      construct(x, opts_haven_labelled_spss("next"))
    Output
      haven::labelled(c(1, -99), labels = c(Missing = -99)) |>
        structure(
          na_values = -99,
          class = c("haven_labelled_spss", "haven_labelled", "vctrs_vctr", "double")
        )
    Code
      construct(structure(1, na_values = NA_real_, class = c("haven_labelled_spss",
        "haven_labelled", "vctrs_vctr", "double")))
    Output
      haven::labelled(1) |>
        structure(
          na_values = NA_real_,
          class = c("haven_labelled_spss", "haven_labelled", "vctrs_vctr", "double")
        )

