test_that("S7_generic, S7 <= 0.2.1", {
  skip_if_not_installed("S7")
  skip_if(with_versions(S7 > "0.2.1"))
  expect_snapshot({
    # examples from `?S7::new_property`
    type_of <- S7::new_generic("type_of", dispatch_args = "x")
    construct(type_of)
    # to avoid issue with changing env addresses
    environment(attr(type_of, "S7_class")) <- .GlobalEnv
    environment(attr(attr(type_of, "S7_class"), "constructor")) <- .GlobalEnv
    construct(type_of, opts_S7_generic("next"), opts_function(srcref = TRUE), check = FALSE)
    S7::method(type_of, S7::class_character) <- function(x, ...) "A character vector"
    S7::method(type_of, S7::new_S3_class("data.frame")) <- function(x, ...) "A data frame"
    S7::method(type_of, S7::class_function) <- function(x, ...) "A function"
  })
})

test_that("S7_generic, S7 > 0.2.1", {
  # The class is stored in a reference shared by all generics, so we don't
  # change its environment, check = FALSE because of environment addresses
  skip_if_not_installed("S7")
  skip_if(with_versions(S7 <= "0.2.1"))
  expect_snapshot({
    # examples from `?S7::new_property`
    type_of <- S7::new_generic("type_of", dispatch_args = "x")
    construct(type_of)
    construct(type_of, opts_S7_generic("next"), opts_function(srcref = TRUE), check = FALSE)
    S7::method(type_of, S7::class_character) <- function(x, ...) "A character vector"
    S7::method(type_of, S7::new_S3_class("data.frame")) <- function(x, ...) "A data frame"
    S7::method(type_of, S7::class_function) <- function(x, ...) "A function"
  })
})
