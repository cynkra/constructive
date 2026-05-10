test_that("pairlist", {
  expect_snapshot({
    construct(pairlist(a=1, 2))
    construct(pairlist(a=1, 2), opts_pairlist("pairlist2"))
  })
})
