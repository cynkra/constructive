test_that("raw", {
  # check = FALSE for raw strings to pass tests on older R versions
  expect_snapshot({
    construct(raw(1))
    construct(raw(2))
    construct(raw(10))
    construct(structure(raw(2), foo = 1))

    construct(raw(1), opts_raw(representation = "decimal"))
    construct(raw(2), opts_raw(representation = "decimal"))
    construct(raw(10), opts_raw(representation = "decimal"))

    construct(
      as.raw(c(0x68, 0x65, 0x6c, 0x6c, 0x6f, 0x77, 0x6f, 0x72, 0x6c, 0x64)),
      opts_raw("charToRaw")
    )
  })
})
