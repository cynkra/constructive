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

