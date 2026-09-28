test_that("vctrs_rcrd", {
  skip_if_not_installed("vctrs")
  expect_snapshot({
    construct(vctrs::new_rcrd(list(x = 1:2, y = c("a", "b"))))
    construct(vctrs::new_rcrd(list(x = 1:2, y = c("a", "b"))), opts_vctrs_rcrd("next"))
    construct(vctrs::new_rcrd(list(x = 1:2, y = c("a", "b"))), opts_vctrs_rcrd("list"))
    # empty
    construct(vctrs::new_rcrd(list(x = integer())))
    # subclass and additional attributes
    construct(vctrs::new_rcrd(list(x = c(1, NA)), unit = "cm", class = "my_rcrd"))
    # attribute partially matching `fields` is repaired
    construct(structure(vctrs::new_rcrd(list(x = 1), class = "my_rcrd"), fi = 1))
    # corrupted: fields of different sizes
    construct(structure(list(x = 1, y = 1:2), class = c("vctrs_rcrd", "vctrs_vctr")))
  })
})
