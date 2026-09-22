# col_spec

    Code
      construct(readr::cols())
    Output
      readr::cols()
    Code
      construct(readr::cols(a = readr::col_double(), b = "c", c = readr::col_date(
        "%Y")))
    Output
      readr::cols(
        a = readr::col_double(),
        b = readr::col_character(),
        c = readr::col_date(format = "%Y")
      )
    Code
      construct(readr::cols(readr::col_double(), readr::col_integer()))
    Output
      readr::cols(readr::col_double(), readr::col_integer())
    Code
      construct(readr::cols(a = "i", .default = readr::col_character()))
    Output
      readr::cols(a = readr::col_integer(), .default = readr::col_character())
    Code
      construct(readr::cols_only(a = "d", b = "c"))
    Output
      readr::cols_only(a = readr::col_double(), b = readr::col_character())
    Code
      construct(readr::cols(a = "d", .default = readr::col_skip(), .delim = ";"))
    Output
      readr::cols(a = readr::col_double(), .default = readr::col_skip(), .delim = ";")
    Code
      construct(readr::spec(readr::read_csv(I("a,b\n1,x"), show_col_types = FALSE)))
    Output
      readr::cols(a = readr::col_double(), b = readr::col_character(), .delim = ",")
    Code
      construct(structure(readr::cols(a = "d"), foo = "bar"))
    Output
      readr::cols(a = readr::col_double()) |>
        structure(foo = "bar")
    Code
      construct(structure(list(cols = list(a = 1), default = readr::col_guess(),
      delim = NULL), class = "col_spec"))
    Output
      list(cols = list(a = 1), default = readr::col_guess(), delim = NULL) |>
        structure(class = "col_spec")
    Code
      construct(readr::cols(a = "d"), opts_col_spec("next"))
    Output
      list(cols = list(a = readr::col_double()), default = readr::col_guess(), delim = NULL) |>
        structure(class = "col_spec")
    Code
      construct(readr::cols(a = "d"), opts_col_spec("list"))
    Output
      list(cols = list(a = readr::col_double()), default = readr::col_guess(), delim = NULL) |>
        structure(class = "col_spec")

