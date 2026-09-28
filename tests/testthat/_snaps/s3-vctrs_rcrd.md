# vctrs_rcrd

    Code
      construct(vctrs::new_rcrd(list(x = 1:2, y = c("a", "b"))))
    Output
      vctrs::new_rcrd(list(x = 1:2, y = c("a", "b")))
    Code
      construct(vctrs::new_rcrd(list(x = 1:2, y = c("a", "b"))), opts_vctrs_rcrd(
        "next"))
    Output
      list(x = 1:2, y = c("a", "b")) |>
        structure(class = c("vctrs_rcrd", "vctrs_vctr"))
    Code
      construct(vctrs::new_rcrd(list(x = 1:2, y = c("a", "b"))), opts_vctrs_rcrd(
        "list"))
    Output
      list(x = 1:2, y = c("a", "b")) |>
        structure(class = c("vctrs_rcrd", "vctrs_vctr"))
    Code
      construct(vctrs::new_rcrd(list(x = integer())))
    Output
      vctrs::new_rcrd(list(x = integer(0)))
    Code
      construct(vctrs::new_rcrd(list(x = c(1, NA)), unit = "cm", class = "my_rcrd"))
    Output
      vctrs::new_rcrd(list(x = c(1, NA)), unit = "cm", class = "my_rcrd")
    Code
      construct(structure(vctrs::new_rcrd(list(x = 1), class = "my_rcrd"), fi = 1))
    Output
      vctrs::new_rcrd(list(x = 1), class = "my_rcrd") |>
        structure(fi = 1)
    Code
      construct(structure(list(x = 1, y = 1:2), class = c("vctrs_rcrd", "vctrs_vctr")))
    Output
      list(x = 1, y = 1:2) |>
        structure(class = c("vctrs_rcrd", "vctrs_vctr"))

