test_that("Duration", {
  skip_if_not_installed("lubridate")
  expect_snapshot({
    construct(lubridate::dweeks(2))
    construct(lubridate::ddays(3))
    construct(lubridate::dhours(1))
    construct(lubridate::dminutes(90))
    construct(lubridate::dseconds(1.5))
    # only seconds work for all the elements
    construct(lubridate::dseconds(c(3600, 1)))
    # `NA` elements don't constrain the choice of the unit
    construct(lubridate::dseconds(c(NA, 86400)))
    construct(lubridate::dseconds(c(NA, NA)))
    # non finite elements are only whole numbers of seconds
    construct(lubridate::dseconds(c(Inf, 86400)))
    # a larger unit would be arbitrary
    construct(lubridate::dseconds(0))
    construct(lubridate::duration())
    # approximations, we don't use `dyears()` and `dmonths()`
    construct(lubridate::dyears(1))
    construct(lubridate::dmonths(1))
    construct(structure(lubridate::dminutes(1), foo = "bar"))
    construct(lubridate::ddays(1:3), opts_Duration("default"))
    construct(lubridate::ddays(1:3), opts_Duration("dseconds"))
    construct(lubridate::ddays(1:3), opts_Duration("duration"))
    construct(lubridate::ddays(1:3), opts_Duration("next"))
  })
})
