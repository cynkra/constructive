test_that("Duration", {
  skip_if_not_installed("lubridate")
  expect_snapshot({
    construct(lubridate::dseconds(c(3.5, NA)))
    construct(lubridate::ddays(1:3))
    construct(lubridate::duration())
    construct(structure(lubridate::dminutes(1), foo = "bar"))
    construct(lubridate::ddays(1:3), opts_Duration("dseconds"))
    construct(lubridate::ddays(1:3), opts_Duration("duration"))
    construct(lubridate::ddays(1:3), opts_Duration("next"))
  })
})
