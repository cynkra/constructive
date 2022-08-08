test_that("multiplication works", {
  expect_snapshot({
    # by default no scientific notation
    construct(10000)
    # by default scientific notation
    construct(100000)
    # not truncated
    construct(.1000000000000001)
    # truncated
    construct(.10000000000000001)
    # by default scientific notation
    construct(.0000000000000011)
    # max_atomic
    construct(c(1, 2, 3), max_atomic = 0)
    construct(c(1, 2, 3), max_atomic = 2)
    # don't print useless extra digits (thanks to format(x, digits = 15))
    construct(0.07)
  })
})
