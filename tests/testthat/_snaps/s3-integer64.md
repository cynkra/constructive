# integer64

    Code
      construct(bit64::as.integer64(c("-1000", NA, "100000")))
    Output
      bit64::as.integer64(c("-1000", NA, "100000"))
    Code
      construct(bit64::as.integer64(NA))
    Output
      bit64::as.integer64(NA_character_)
    Code
      construct(bit64::as.integer64(c("0", NA, "-1", "-9223372036854775807")))
    Output
      bit64::as.integer64(c("0", NA, "-1", "-9223372036854775807"))

