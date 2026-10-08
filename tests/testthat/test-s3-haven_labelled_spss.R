test_that("haven_labelled_spss", {
  skip_if_not_installed("haven")
  expect_snapshot({
    construct(haven::labelled_spss(
      c(1, 2, -99),
      labels = c(Male = 1, Missing = -99),
      na_values = -99,
      na_range = c(90, Inf),
      label = "Sex"
    ))
    construct(haven::labelled_spss(1:2))
    construct(haven::labelled_spss(character()))
    construct(haven::labelled_spss(c("a", "Z", NA), labels = c(A = "a"), na_values = "Z", na_range = c("X", "Z")))
    construct(structure(haven::labelled_spss(c(a = 1, b = -99), na_values = -99), foo = "bar"))
    x <- haven::labelled_spss(c(1, -99), labels = c(Missing = -99), na_values = -99)
    construct(x, opts_haven_labelled_spss("labelled_spss"))
    construct(x, opts_haven_labelled_spss("next"))
    # corrupted
    construct(structure(1, na_values = NA_real_, class = c("haven_labelled_spss", "haven_labelled", "vctrs_vctr", "double")))
  })
})
