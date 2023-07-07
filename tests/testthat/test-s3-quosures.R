test_that("quosures", {
  expect_pipe_snapshot({
    construct(
      rlang::new_quosures(list(
        rlang::new_quosure(quote(x)),
        rlang::new_quosure(quote(x), asNamespace("constructive"))
      ))
    )

    construct(
      rlang::new_quosures(list(
        rlang::new_quosure(quote(x)),
        rlang::new_quosure(quote(x), asNamespace("constructive"))
      )),
      opts_quosures("next")
    )

    construct(
      rlang::new_quosures(list(
        a = rlang::new_quosure(quote(x)),
        rlang::new_quosure(quote(x), asNamespace("constructive"))
      )),
      opts_environment("list2env")
    )
  })
})
