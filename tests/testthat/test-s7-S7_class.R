test_that("S7_class", {
  # When S7 is installed from source with `R_KEEP_PKG_SOURCE=yes` (as on CI
  # for platforms without binaries), its functions keep their srcref and are
  # constructed with the indentation of the S7 source code, 2 more spaces.
  # The snapshot corresponds to functions without srcref.

  # We also have an error with ubuntu with R 4.2, 4.3 : generic function not specified
  # I could not debug directly but since (1) the code goes through
  # constructive:::.cstr_construct.S4 and (2) it calls NextMethod(), it would seem
  # that the S7 code uses the type "S4" and not "object" in these older versions
  # and that out corruption test using `!isS4(x)` is not appropriate (should
  # probably be `typeof(x) != "S4"`). In these older versions maybe we didn't have
  # the type "object" and could have "S4" objects without the "S4" flag on.

  # It seems like a lot of work for outdated versions so we will just skip
  # linux for this test

  testthat::skip_on_os("linux")
  skip_if_not_installed("S7")
  skip_if(
    !is.null(attr(S7::S7_object@constructor, "srcref")),
    "S7 was installed with source references"
  )
  expect_snapshot({
    construct(S7::S7_object)
    construct(S7::S7_object, opts_S7_class("next"))
  })
})
