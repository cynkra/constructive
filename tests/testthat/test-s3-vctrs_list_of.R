test_that("vctrs_list_of", {
  expect_pipe_snapshot({
    construct(vctrs::vec_c(vctrs::list_of(1, 2), vctrs::list_of(FALSE, TRUE)))
    construct(vctrs::vec_c(vctrs::list_of(1, 2), vctrs::list_of(FALSE, TRUE)), opts_vctrs_list_of("list"))
  })
})
