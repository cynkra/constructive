# date_names

    Code
      construct(readr::date_names_lang("en"))
    Output
      readr::date_names_lang("en")
    Code
      construct(readr::date_names_lang("fr"))
    Output
      readr::date_names_lang("fr")
    Code
      construct(readr::date_names_lang("en"), opts_date_names("date_names"))
    Output
      readr::date_names(
        mon = c(
          "January", "February", "March", "April", "May", "June", "July", "August",
          "September", "October", "November", "December"
        ),
        mon_ab = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
        day = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"),
        day_ab = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
      )
    Code
      construct(readr::date_names(month.name, month.abb, day = c("Su", "Mo", "Tu",
        "We", "Th", "Fr", "Sa")))
    Output
      readr::date_names(
        mon = c(
          "January", "February", "March", "April", "May", "June", "July", "August",
          "September", "October", "November", "December"
        ),
        mon_ab = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
        day = c("Su", "Mo", "Tu", "We", "Th", "Fr", "Sa")
      )
    Code
      construct(readr::date_names(month.name, day = letters[1:7], am_pm = c("am",
        "pm")))
    Output
      readr::date_names(
        mon = c(
          "January", "February", "March", "April", "May", "June", "July", "August",
          "September", "October", "November", "December"
        ),
        day = c("a", "b", "c", "d", "e", "f", "g"),
        am_pm = c("am", "pm")
      )
    Code
      construct(structure(readr::date_names_lang("en"), foo = "bar"))
    Output
      readr::date_names(
        mon = c(
          "January", "February", "March", "April", "May", "June", "July", "August",
          "September", "October", "November", "December"
        ),
        mon_ab = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
        day = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"),
        day_ab = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
      ) |>
        structure(foo = "bar")
    Code
      construct(structure(list(mon = month.name), class = "date_names"))
    Output
      list(
        mon = c(
          "January", "February", "March", "April", "May", "June", "July", "August",
          "September", "October", "November", "December"
        )
      ) |>
        structure(class = "date_names")
    Code
      construct(readr::date_names_lang("en"), opts_date_names("next"))
    Output
      list(
        mon = c(
          "January", "February", "March", "April", "May", "June", "July", "August",
          "September", "October", "November", "December"
        ),
        mon_ab = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
        day = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"),
        day_ab = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"),
        am_pm = c("AM", "PM")
      ) |>
        structure(class = "date_names")
    Code
      construct(readr::date_names_lang("en"), opts_date_names("list"))
    Output
      list(
        mon = c(
          "January", "February", "March", "April", "May", "June", "July", "August",
          "September", "October", "November", "December"
        ),
        mon_ab = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
        day = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"),
        day_ab = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"),
        am_pm = c("AM", "PM")
      ) |>
        structure(class = "date_names")

# date_names with non ASCII month names on UTF-8 system

    Code
      construct(readr::date_names_lang("fr"), opts_date_names("date_names"))
    Output
      readr::date_names(
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
    Code
      construct(readr::date_names_lang("fr"), opts_date_names("list"))
    Output
      list(
        mon = c(
          "janvier", "f\U{E9}vrier", "mars", "avril", "mai", "juin", "juillet",
          "ao\U{FB}t", "septembre", "octobre", "novembre", "d\U{E9}cembre"
        ),
        mon_ab = c(
          "janv.", "f\U{E9}vr.", "mars", "avr.", "mai", "juin", "juil.", "ao\U{FB}t",
          "sept.", "oct.", "nov.", "d\U{E9}c."
        ),
        day = c("dimanche", "lundi", "mardi", "mercredi", "jeudi", "vendredi", "samedi"),
        day_ab = c("dim.", "lun.", "mar.", "mer.", "jeu.", "ven.", "sam."),
        am_pm = c("AM", "PM")
      ) |>
        structure(class = "date_names")

