test_that("quosures", {
  expect_snapshot({
    construct(
      rlang::new_quosures(list(
        rlang::new_quosure(quote(x)),
        rlang::new_quosure(quote(x), new.env())
      ))
    )

    construct(
      rlang::new_quosures(list(
        a = rlang::new_quosure(quote(x)),
        rlang::new_quosure(quote(x), new.env())
      ))
    )
  })
})
