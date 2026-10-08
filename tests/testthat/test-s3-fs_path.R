test_that("fs_path", {
  skip_if_not_installed("fs")
  expect_snapshot({
    construct(fs::path("a/b"))
    construct(fs::path(c("a/b", "~/c.txt", NA)))
    construct(fs::path(character()))
    construct(structure(fs::path(c("a", "b")), names = c("x", "y")))
    construct(structure(fs::path("a"), foo = "bar"))
    construct(fs::path("a/b"), opts_fs_path("as_fs_path"))
    construct(fs::path("a/b"), opts_fs_path("next"))
    # not tidy, can't be built with fs::path()
    construct(structure("a//b/", class = c("fs_path", "character")))
  })
})
