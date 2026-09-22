# fs_bytes

    Code
      construct(fs::as_fs_bytes(1024))
    Output
      fs::as_fs_bytes(1024)
    Code
      construct(fs::as_fs_bytes(c(0, 10485760, 1.5, NA, Inf)))
    Output
      fs::as_fs_bytes(c(0, 10485760, 1.5, NA, Inf))
    Code
      construct(fs::as_fs_bytes(10L))
    Output
      fs::as_fs_bytes(10L)
    Code
      construct(fs::as_fs_bytes(numeric()))
    Output
      fs::as_fs_bytes(numeric(0))
    Code
      construct(fs::as_fs_bytes(c(a = 1, b = 2)))
    Output
      fs::as_fs_bytes(c(a = 1, b = 2))
    Code
      construct(structure(fs::as_fs_bytes(1), foo = "bar"))
    Output
      fs::as_fs_bytes(1) |>
        structure(foo = "bar")
    Code
      construct(fs::as_fs_bytes(1024), opts_fs_bytes("next"))
    Output
      1024 |>
        structure(class = c("fs_bytes", "numeric"))

