test_that("all_classes contains all classes", {
  # infer all implemented classes from object names
  all_methods <- ls(asNamespace("constructive"), pattern = "^[.]cstr_construct[.]", all.names = TRUE)
  high_level_methods  <- all_methods[sapply(all_methods, function(x) sum(startsWith(all_methods, paste0(x, "."))) > 1)]
  implemented_classes <- sub("^[.]cstr_construct[.]", "", high_level_methods)
  to_add_to_all_classes <- setdiff(implemented_classes, unlist(all_classes))
  expect_length(to_add_to_all_classes, 0)
})

