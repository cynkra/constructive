# xts

    Code
      mat <- matrix(c(50.0397819115463, 50.2304961977954, 50.420955209067,
        50.3734680543285, 50.2443255196795, 50.1321122972067, 50.0355467742705,
        49.9948860954217), nrow = 2L, ncol = 4L, dimnames = list(c("2007-01-02",
        "2007-01-03"), c("Open", "High", "Low", "Close")))
      x <- xts::as.xts(mat)
      construct(x)
    Output
      xts::as.xts(matrix(
        c(
          50.0397819115463, 50.2304961977954, 50.420955209067, 50.3734680543285,
          50.2443255196795, 50.1321122972067, 50.0355467742705, 49.9948860954217
        ),
        nrow = 2L,
        ncol = 4L,
        dimnames = list(c("2007-01-02", "2007-01-03"), c("Open", "High", "Low", "Close"))
      ))
    Code
      construct(x, opts_xts("as.xts.data.frame"))
    Output
      xts::as.xts(data.frame(
        Open = c(50.0397819115463, 50.2304961977954),
        High = c(50.420955209067, 50.3734680543285),
        Low = c(50.2443255196795, 50.1321122972067),
        Close = c(50.0355467742705, 49.9948860954217),
        row.names = c("2007-01-02", "2007-01-03")
      ))
    Code
      construct(x, opts_xts("xts"))
    Output
      matrix(
        c(
          50.0397819115463, 50.2304961977954, 50.420955209067, 50.3734680543285,
          50.2443255196795, 50.1321122972067, 50.0355467742705, 49.9948860954217
        ),
        nrow = 2L,
        ncol = 4L,
        dimnames = list(NULL, c("Open", "High", "Low", "Close"))
      ) |>
        xts::xts(
          order.by = as.POSIXct(c("2007-01-02", "2007-01-03")) |>
            structure(tclass = c("POSIXct", "POSIXt"))
        )
    Code
      construct(x, opts_xts(".xts"))
    Output
      matrix(
        c(
          50.0397819115463, 50.2304961977954, 50.420955209067, 50.3734680543285,
          50.2443255196795, 50.1321122972067, 50.0355467742705, 49.9948860954217
        ),
        nrow = 2L,
        ncol = 4L,
        dimnames = list(NULL, c("Open", "High", "Low", "Close"))
      ) |>
        xts::.xts(
          index = c(1167696000, 1167782400) |>
            structure(tzone = "", tclass = c("POSIXct", "POSIXt"))
        )
    Code
      construct(x, opts_xts("xts"), one_liner = TRUE)
    Output
      xts::xts(matrix(c(50.0397819115463, 50.2304961977954, 50.420955209067, 50.3734680543285, 50.2443255196795, 50.1321122972067, 50.0355467742705, 49.9948860954217), nrow = 2L, ncol = 4L, dimnames = list(NULL, c("Open", "High", "Low", "Close"))), order.by = as.POSIXct(c("2007-01-02", "2007-01-03")) |> structure(tclass = c("POSIXct", "POSIXt")))
    Code
      construct(x, opts_xts(".xts"), one_liner = TRUE)
    Output
      xts::.xts(matrix(c(50.0397819115463, 50.2304961977954, 50.420955209067, 50.3734680543285, 50.2443255196795, 50.1321122972067, 50.0355467742705, 49.9948860954217), nrow = 2L, ncol = 4L, dimnames = list(NULL, c("Open", "High", "Low", "Close"))), index = c(1167696000, 1167782400) |> structure(tzone = "", tclass = c("POSIXct", "POSIXt")))
    Code
      construct(x, opts_xts("next"))
    Output
      matrix(
        c(
          50.0397819115463, 50.2304961977954, 50.420955209067, 50.3734680543285,
          50.2443255196795, 50.1321122972067, 50.0355467742705, 49.9948860954217
        ),
        nrow = 2L,
        ncol = 4L,
        dimnames = list(NULL, c("Open", "High", "Low", "Close"))
      ) |>
        structure(
          index = c(1167696000, 1167782400) |>
            structure(tzone = "", tclass = c("POSIXct", "POSIXt")),
          class = c("xts", "zoo")
        )
    Code
      construct_dput(x)
    Output
      matrix(
        c(
          50.0397819115463, 50.2304961977954, 50.420955209067, 50.3734680543285,
          50.2443255196795, 50.1321122972067, 50.0355467742705, 49.9948860954217
        ),
        nrow = 2L,
        ncol = 4L,
        dimnames = list(NULL, c("Open", "High", "Low", "Close"))
      ) |>
        structure(
          index = c(1167696000, 1167782400) |>
            structure(tzone = "", tclass = c("POSIXct", "POSIXt")),
          class = c("xts", "zoo")
        )
    Code
      construct_base(x)
    Output
      matrix(
        c(
          50.0397819115463, 50.2304961977954, 50.420955209067, 50.3734680543285,
          50.2443255196795, 50.1321122972067, 50.0355467742705, 49.9948860954217
        ),
        nrow = 2L,
        ncol = 4L,
        dimnames = list(NULL, c("Open", "High", "Low", "Close"))
      ) |>
        structure(
          index = c(1167696000, 1167782400) |>
            structure(tzone = "", tclass = c("POSIXct", "POSIXt")),
          class = c("xts", "zoo")
        )

