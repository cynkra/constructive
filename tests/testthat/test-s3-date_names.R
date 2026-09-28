test_that("date_names", {
  skip_if_not_installed("readr")
  expect_snapshot({
    construct(readr::date_names_lang("en"))
    construct(readr::date_names_lang("fr"))
    construct(readr::date_names_lang("en"), opts_date_names("date_names"))
    construct(readr::date_names(month.name, month.abb, day = c("Su", "Mo", "Tu", "We", "Th", "Fr", "Sa")))
    construct(readr::date_names(month.name, day = letters[1:7], am_pm = c("am", "pm")))
    construct(structure(readr::date_names_lang("en"), foo = "bar"))
    # corrupted date_names fall back to list
    construct(structure(list(mon = month.name), class = "date_names"))
    construct(readr::date_names_lang("en"), opts_date_names("next"))
    construct(readr::date_names_lang("en"), opts_date_names("list"))
  })
})

test_that("date_names with non ASCII month names on UTF-8 system", {
  skip_if_not_installed("readr")
  skip_if(!l10n_info()$`UTF-8`)
  expect_snapshot({
    construct(readr::date_names_lang("fr"), opts_date_names("date_names"))
    construct(readr::date_names_lang("fr"), opts_date_names("list"))
  })
})
