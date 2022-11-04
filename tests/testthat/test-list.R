test_that("list", {
  expect_snapshot({
    construct(list(a = 1, b = list(c(1L, 3L), list(.leap.seconds[1:2]))))

    x1 <- as.list(letters[1:4])
    construct(x1)
    construct(x1, opts_list("list2"))

    x2 <- as.list(letters)
    construct(x2)
    construct(x2, opts_list("list2"))

    construct(x2, opts_list(trim = 2)) # fill = "vector"
    construct(x2, opts_list(trim = 26))
    construct(x2, opts_list(trim = 30))
    construct(x2, opts_list(trim = 2, fill = "new_list"))
    construct(x2, opts_list(trim = 2, fill = "+"))
    construct(x2, opts_list(trim = 2, fill = "none"))
    construct(x2, opts_list(trim = 2, fill = "..."))
  })
})
