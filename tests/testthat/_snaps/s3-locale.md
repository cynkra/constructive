# locale

    Code
      construct(readr::locale())
    Output
      readr::locale()
    Code
      construct(readr::locale("fr"))
    Output
      readr::locale(date_names = "fr")
    Code
      construct(readr::locale(), opts_date_names("date_names"))
    Output
      readr::locale(
        date_names = readr::date_names(
          mon = c(
            "January", "February", "March", "April", "May", "June", "July", "August",
            "September", "October", "November", "December"
          ),
          mon_ab = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
          day = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"),
          day_ab = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
        )
      )
    Code
      construct(readr::locale(decimal_mark = ","))
    Output
      readr::locale(decimal_mark = ",")
    Code
      construct(readr::locale(grouping_mark = "."))
    Output
      readr::locale(decimal_mark = ",")
    Code
      construct(readr::locale(grouping_mark = " "))
    Output
      readr::locale(grouping_mark = " ")
    Code
      construct(readr::locale(decimal_mark = ",", grouping_mark = " "))
    Output
      readr::locale(decimal_mark = ",", grouping_mark = " ")
    Code
      construct(readr::locale(date_format = "%d/%m/%Y", time_format = "%H:%M", tz = "Europe/Paris",
        encoding = "latin1"))
    Output
      readr::locale(
        date_format = "%d/%m/%Y",
        time_format = "%H:%M",
        tz = "Europe/Paris",
        encoding = "latin1"
      )
    Code
      construct(readr::locale(date_names = readr::date_names(month.name, day = letters[
        1:7])))
    Output
      readr::locale(
        date_names = readr::date_names(
          mon = c(
            "January", "February", "March", "April", "May", "June", "July", "August",
            "September", "October", "November", "December"
          ),
          day = c("a", "b", "c", "d", "e", "f", "g")
        )
      )
    Code
      construct(structure(readr::locale(), foo = "bar"))
    Output
      readr::locale() |>
        structure(foo = "bar")
    Code
      construct(structure(list(tz = "UTC"), class = "locale"))
    Output
      list(tz = "UTC") |>
        structure(class = "locale")
    Code
      construct(readr::locale("fr"), opts_locale("next"))
    Output
      list(
        date_names = readr::date_names_lang("fr"),
        date_format = "%AD",
        time_format = "%AT",
        decimal_mark = ".",
        grouping_mark = ",",
        tz = "UTC",
        encoding = "UTF-8"
      ) |>
        structure(class = "locale")
    Code
      construct(readr::locale("fr"), opts_locale("list"))
    Output
      list(
        date_names = readr::date_names_lang("fr"),
        date_format = "%AD",
        time_format = "%AT",
        decimal_mark = ".",
        grouping_mark = ",",
        tz = "UTC",
        encoding = "UTF-8"
      ) |>
        structure(class = "locale")

# locale with non ASCII date names on UTF-8 system

    Code
      construct(readr::locale("fr"), opts_date_names("date_names"))
    Output
      readr::locale(
        date_names = readr::date_names(
          mon = c(
            "janvier", "f\U{E9}vrier", "mars", "avril", "mai", "juin", "juillet",
            "ao\U{FB}t", "septembre", "octobre", "novembre", "d\U{E9}cembre"
          ),
          mon_ab = c(
            "janv.", "f\U{E9}vr.", "mars", "avr.", "mai", "juin", "juil.", "ao\U{FB}t",
            "sept.", "oct.", "nov.", "d\U{E9}c."
          ),
          day = c("dimanche", "lundi", "mardi", "mercredi", "jeudi", "vendredi", "samedi"),
          day_ab = c("dim.", "lun.", "mar.", "mer.", "jeu.", "ven.", "sam.")
        )
      )

