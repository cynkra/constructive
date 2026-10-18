test_that("S7_union", {
  expect_snapshot({
    construct(S7::class_vector)
    construct(S7::class_vector, opts_S7_union("|"))
    construct(S7::class_vector, opts_S7_union("new_union"))
    construct(S7::class_vector, opts_S7_union("next"))
  })
})

test_that("S7_union, S7 <= 0.2.1", {
  skip_if_not_installed("S7")
  skip_if(with_versions(S7 > "0.2.1"))
  expect_snapshot({
    # check = FALSE because of changing environment address issues
    construct(S7::class_vector, opts_S7_union("next"), opts_S7_base_class("next"), check = FALSE)
  })
})

test_that("S7_union, S7 > 0.2.1", {
  # the base classes are defined differently
  skip_if_not_installed("S7")
  skip_if(with_versions(S7 <= "0.2.1"))
  expect_snapshot({
    construct(S7::class_vector, opts_S7_union("next"), opts_S7_base_class("next"), check = FALSE)
  })
})


class_logical <- list(
  class = "logical",
  constructor_name = "logical",
  constructor = (function(.data = logical(0)) {
    .data
  }) |>
    (`environment<-`)(baseenv()),
  validator = (function(object) {
    if (base_class(object) != "logical") {
      sprintf("Underlying data must be logical not <%s>", base_class(object))
    }
  }) |>
    (`environment<-`)(baseenv())
) |>
  structure(class = "S7_base_class")
