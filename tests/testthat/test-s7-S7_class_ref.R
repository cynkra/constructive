test_that("S7_class_ref", {
  # S7 <= 0.2.2 doesn't use class references
  testthat::skip_on_os("linux")
  skip_if_not_installed("S7")
  skip_if(with_versions(S7 <= "0.2.2"))
  expect_snapshot({
    Dog <- evalq(
      S7::new_class("Dog", properties = list(name = S7::class_character)),
      .GlobalEnv
    )
    # check = FALSE because the check evaluates the code in the test environment
    # where `new_class()` infers the package "constructive"
    construct(Dog(name = "x"), check = FALSE)
    construct(attr(Dog(name = "x"), "_S7_class"), check = FALSE)
    construct(attr(Dog(name = "x"), "_S7_class"), opts_S7_class_ref("next"), check = FALSE)
  })
})
