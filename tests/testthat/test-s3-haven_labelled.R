test_that("haven_labelled", {
  skip_if_not_installed("haven")
  expect_snapshot({
    construct(haven::labelled(c(1, 2, NA), labels = c(Male = 1, Female = 2), label = "Sex"))
    construct(haven::labelled(1:3))
    construct(haven::labelled(double()))
    construct(haven::labelled(c("a", "b"), labels = c(A = "a")))
    construct(haven::labelled(c(a = 1, b = 2), labels = c(x = 1)))
    construct(structure(haven::labelled(1:3, labels = c(A = 1L)), foo = "bar"))
    construct(data.frame(sex = haven::labelled(c(1, 2), labels = c(Male = 1, Female = 2))))
    construct(haven::labelled(c(1, 2), labels = c(Male = 1)), opts_haven_labelled("labelled"))
    construct(haven::labelled(c(1, 2), labels = c(Male = 1)), opts_haven_labelled("next"))
    # corrupted
    construct(structure(TRUE, class = c("haven_labelled", "vctrs_vctr", "logical")))
  })
})
