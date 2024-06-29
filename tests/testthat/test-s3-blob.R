test_that("blob", {
  expect_snapshot({
  construct(blob::as_blob(c("hello", "world")))
  construct(blob::as_blob(c("hello", "world")), opts_blob("new_blob"))
  construct(blob::as_blob(c("hello", "world")), opts_blob("as_blob"))
  construct(blob::as_blob(c("hello", "world")), opts_blob("next"))

  construct(blob::blob(as.raw(0x30)))
  construct(blob::blob(as.raw(0x30)), opts_blob("new_blob"))
  construct(blob::blob(as.raw(0x30)), opts_blob("as_blob"))

  construct(blob::blob(as.raw(0x00)))
  construct(blob::blob(as.raw(0x00)), opts_blob("new_blob"))
  # fall back correctly
  construct(blob::blob(as.raw(0x00)), opts_blob("as_blob"))
  })
})

