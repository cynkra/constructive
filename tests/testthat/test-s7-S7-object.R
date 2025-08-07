test_that("S7_object", {
  expect_snapshot({
    construct(S7::S7_object())
    construct(S7::S7_object(), opts_S7_object("next"))
    })
})
