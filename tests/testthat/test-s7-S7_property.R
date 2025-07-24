test_that("S7_property", {
  expect_snapshot({
    # examples from `?S7::new_property`
    slices <- S7::new_property(S7::class_numeric, default = 10)
    construct(slices)
    construct(slices, opts_S7_property("next"))
    now <- S7::new_property(getter = function(self) Sys.time())
    # to avoid changing environment adress issue
    environment(now$getter) <- .GlobalEnv
    construct(now)
    construct(now, opts_S7_property("next"))
  })
})
