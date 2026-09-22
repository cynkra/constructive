test_that("vctrs_vctr", {
  skip_if_not_installed("vctrs")
  expect_snapshot({
    construct(vctrs::new_vctr(c(a = 1, b = NA), unit = "cm", class = "my_vctr"))
    construct(vctrs::new_vctr(c(a = 1, b = NA), unit = "cm", class = "my_vctr"), opts_vctrs_vctr("next"))
    # empty
    construct(vctrs::new_vctr(character()))
    # inherit base type
    construct(vctrs::new_vctr(1:3, inherit_base_type = TRUE))
    construct(vctrs::new_vctr(list(1, "a")))
    # attributes clashing with `new_vctr()` args are repaired
    construct(structure(vctrs::new_vctr(1:3), .d = 1, inherit_base_type = 2))
    # corrupted: list not inheriting from base type
    construct(structure(list(1, 2), class = "vctrs_vctr"))
    # classes inheriting from vctrs_vctr
    construct(vctrs::list_of(1, 2), opts_vctrs_list_of("next"))
  })
})
