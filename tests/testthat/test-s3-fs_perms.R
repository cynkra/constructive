test_that("fs_perms", {
  skip_if_not_installed("fs")
  expect_snapshot({
    construct(fs::as_fs_perms("rw-r--r--"))
    construct(fs::as_fs_perms(c("644", "4755")))
    construct(fs::as_fs_perms(as.octmode(c("644", "755", "0", "7777"))))
    construct(fs::as_fs_perms(integer()))
    construct(structure(fs::as_fs_perms(c("644", "755")), names = c("a", "b")))
    construct(structure(fs::as_fs_perms("644"), foo = "bar"))
    construct(fs::as_fs_perms(NA_integer_))
    construct(fs::as_fs_perms("644"), opts_fs_perms("next"))
  })
})
