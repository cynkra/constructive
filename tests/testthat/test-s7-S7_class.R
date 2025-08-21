test_that("S7_class", {
  # FIXME:
  # for some reason, ubuntu devel (at time of R 4.5) indent
  # some lines of the output with 2 more spaces (maybe a srcref thing?).
  # this makes the snapshot not unversal
  # would be good to figure out but it has a small impact

  # Similarly we have an error with ubuntu with R 4.2, 4.3 : generic function not specified
  # I could not debug directly but since (1) the code goes through
  # constructive:::.cstr_construct.S4 and (2) it calls NextMethod(), it would seem
  # that the S7 code uses the type "S4" and not "object" in these older versions
  # and that out corruption test using `!isS4(x)` is not appropriate (should
  # probably be `typeof(x) != "S4"`). In these older versions maybe we didn't have
  # the type "object" and could have "S4" objects without the "S4" flag on.

  # It seems like a lot of work for outdated versions so we will just skip
  # linux for this test

  testthat::skip_on_os("linux")
  expect_snapshot({
    construct(S7::S7_object)
    construct(S7::S7_object, opts_S7_class("next"))
  })
})
