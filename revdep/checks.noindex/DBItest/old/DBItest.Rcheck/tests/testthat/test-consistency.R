test_that("no unnamed specs", {
  tests <- compact(spec_all)
  vicinity <- NULL
  if (any(names(tests) == "")) {
    vicinity <- sort(unique(unlist(
      map(which(names(tests) == ""), "+", -1:1)
    )))
    vicinity <- vicinity[names(tests)[vicinity] != ""]
  }
  expect_null(vicinity)
})

test_that("no duplicate spec names expect known exceptions", {
  all_names <- names(spec_all)

  dupe_names <- unique(all_names[duplicated(all_names)])
  expect_equal(dupe_names, rep("", length(dupe_names)))
})

test_that("all specs used", {
  env <- asNamespace("DBItest")
  defined_spec_names <- ls(env, pattern = "^spec_")
  defined_specs <- mget(defined_spec_names, env)
  defined_spec_names <- unlist(sapply(defined_specs, names), use.names = FALSE)
  new_names <- setdiff(defined_spec_names, names(spec_all))
  expect_equal(new_names, rep("", length(new_names)))
})
