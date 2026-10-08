# fs_path

    Code
      construct(fs::path("a/b"))
    Output
      fs::path("a/b")
    Code
      construct(fs::path(c("a/b", "~/c.txt", NA)))
    Output
      fs::path(c("a/b", "~/c.txt", NA))
    Code
      construct(fs::path(character()))
    Output
      fs::path(character(0))
    Code
      construct(structure(fs::path(c("a", "b")), names = c("x", "y")))
    Output
      fs::path(c("a", "b")) |>
        structure(names = c("x", "y"))
    Code
      construct(structure(fs::path("a"), foo = "bar"))
    Output
      fs::path("a") |>
        structure(foo = "bar")
    Code
      construct(fs::path("a/b"), opts_fs_path("as_fs_path"))
    Output
      fs::as_fs_path("a/b")
    Code
      construct(fs::path("a/b"), opts_fs_path("next"))
    Output
      "a/b" |>
        structure(class = c("fs_path", "character"))
    Code
      construct(structure("a//b/", class = c("fs_path", "character")))
    Output
      "a//b/" |>
        structure(class = c("fs_path", "character"))

