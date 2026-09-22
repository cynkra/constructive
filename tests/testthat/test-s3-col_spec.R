test_that("col_spec", {
  skip_if_not_installed("readr")
  expect_snapshot({
    construct(readr::cols())
    construct(readr::cols(a = readr::col_double(), b = "c", c = readr::col_date("%Y")))
    construct(readr::cols(readr::col_double(), readr::col_integer()))
    construct(readr::cols(a = "i", .default = readr::col_character()))
    construct(readr::cols_only(a = "d", b = "c"))
    # cols_only() doesn't support `.delim`
    construct(readr::cols(a = "d", .default = readr::col_skip(), .delim = ";"))
    # as created by read_csv()
    construct(readr::spec(readr::read_csv(I("a,b\n1,x"), show_col_types = FALSE)))
    construct(structure(readr::cols(a = "d"), foo = "bar"))
    # corrupted col_spec falls back to list
    construct(structure(list(cols = list(a = 1), default = readr::col_guess(), delim = NULL), class = "col_spec"))
    construct(readr::cols(a = "d"), opts_col_spec("next"))
    construct(readr::cols(a = "d"), opts_col_spec("list"))
  })
})
