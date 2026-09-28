test_that("fs_bytes", {
  skip_if_not_installed("fs")
  expect_snapshot({
    construct(fs::as_fs_bytes(1024))
    construct(fs::as_fs_bytes(c(0, 10485760, 1.5, NA, Inf)))
    construct(fs::as_fs_bytes(10L))
    construct(fs::as_fs_bytes(numeric()))
    construct(fs::as_fs_bytes(c(a = 1, b = 2)))
    construct(structure(fs::as_fs_bytes(1), foo = "bar"))
    construct(fs::as_fs_bytes(1024), opts_fs_bytes("next"))
  })
})
