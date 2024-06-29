# blob

    Code
      construct(blob::as_blob(c("hello", "world")))
    Output
      blob::blob(
        as.raw(c(0x68, 0x65, 0x6c, 0x6c, 0x6f)),
        as.raw(c(0x77, 0x6f, 0x72, 0x6c, 0x64))
      )
    Code
      construct(blob::as_blob(c("hello", "world")), opts_blob("new_blob"))
    Output
      blob::new_blob(
        list(
          as.raw(c(0x68, 0x65, 0x6c, 0x6c, 0x6f)),
          as.raw(c(0x77, 0x6f, 0x72, 0x6c, 0x64))
        )
      )
    Code
      construct(blob::as_blob(c("hello", "world")), opts_blob("as_blob"))
    Output
      blob::as_blob(c("hello", "world"))
    Code
      construct(blob::as_blob(c("hello", "world")), opts_blob("next"))
    Output
      vctrs::list_of(
        as.raw(c(0x68, 0x65, 0x6c, 0x6c, 0x6f)),
        as.raw(c(0x77, 0x6f, 0x72, 0x6c, 0x64)),
        .ptype = raw(0)
      ) |>
        structure(class = c("blob", "vctrs_list_of", "vctrs_vctr", "list"))
    Code
      construct(blob::blob(as.raw(48)))
    Output
      blob::blob(as.raw(0x30))
    Code
      construct(blob::blob(as.raw(48)), opts_blob("new_blob"))
    Output
      blob::new_blob(list(as.raw(0x30)))
    Code
      construct(blob::blob(as.raw(48)), opts_blob("as_blob"))
    Output
      blob::as_blob("0")
    Code
      construct(blob::blob(as.raw(0)))
    Output
      blob::blob(as.raw(0x00))
    Code
      construct(blob::blob(as.raw(0)), opts_blob("new_blob"))
    Output
      blob::new_blob(list(as.raw(0x00)))
    Code
      construct(blob::blob(as.raw(0)), opts_blob("as_blob"))
    Output
      blob::blob(as.raw(0x00))

