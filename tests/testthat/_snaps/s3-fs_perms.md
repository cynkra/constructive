# fs_perms

    Code
      construct(fs::as_fs_perms("rw-r--r--"))
    Output
      fs::as_fs_perms("644")
    Code
      construct(fs::as_fs_perms(c("644", "4755")))
    Output
      fs::as_fs_perms(c("644", "4755"))
    Code
      construct(fs::as_fs_perms(as.octmode(c("644", "755", "0", "7777"))))
    Output
      fs::as_fs_perms(as.octmode(c("644", "755", "0", "7777")))
    Code
      construct(fs::as_fs_perms(integer()))
    Output
      fs::as_fs_perms(as.octmode(character(0)))
    Code
      construct(structure(fs::as_fs_perms(c("644", "755")), names = c("a", "b")))
    Output
      fs::as_fs_perms(c("644", "755")) |>
        structure(names = c("a", "b"))
    Code
      construct(structure(fs::as_fs_perms("644"), foo = "bar"))
    Output
      fs::as_fs_perms("644") |>
        structure(foo = "bar")
    Code
      construct(fs::as_fs_perms(NA_integer_))
    Output
      NA_integer_ |>
        structure(class = c("fs_perms", "integer"))
    Code
      construct(fs::as_fs_perms("644"), opts_fs_perms("next"))
    Output
      420L |>
        structure(class = c("fs_perms", "integer"))

