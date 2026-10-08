test_that("locale", {
  skip_if_not_installed("readr")
  expect_snapshot({
    construct(readr::locale())
    construct(readr::locale("fr"))
    construct(readr::locale(), opts_date_names("date_names"))
    construct(readr::locale(decimal_mark = ","))
    construct(readr::locale(grouping_mark = "."))
    construct(readr::locale(grouping_mark = " "))
    construct(readr::locale(decimal_mark = ",", grouping_mark = " "))
    construct(readr::locale(
      date_format = "%d/%m/%Y", time_format = "%H:%M", tz = "Europe/Paris", encoding = "latin1"
    ))
    construct(readr::locale(date_names = readr::date_names(month.name, day = letters[1:7])))
    construct(structure(readr::locale(), foo = "bar"))
    # corrupted locale falls back to list
    construct(structure(list(tz = "UTC"), class = "locale"))
    construct(readr::locale("fr"), opts_locale("next"))
    construct(readr::locale("fr"), opts_locale("list"))
  })
})

test_that("locale with non ASCII date names on UTF-8 system", {
  skip_if_not_installed("readr")
  skip_if(!l10n_info()$`UTF-8`)
  expect_snapshot({
    construct(readr::locale("fr"), opts_date_names("date_names"))
  })
})
