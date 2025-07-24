# S7_S3_class

    Code
      S3_Date <- S7::new_S3_class("Date", function(.data = integer()) {
        .Date(.data)
      }, function(self) {
        if (!is.numeric(self)) {
          "Underlying data must be numeric"
        }
      })
      environment(S3_Date$validator) <- .GlobalEnv
      environment(S3_Date$constructor) <- .GlobalEnv
      construct(S3_Date)
    Output
      S7::new_S3_class(
        "Date",
        validator = (function(self) {
          if (!is.numeric(self)) {
            "Underlying data must be numeric"
          }
        }) |>
          (`environment<-`)(.GlobalEnv),
        constructor = (function(.data = integer()) {
          .Date(.data)
        }) |>
          (`environment<-`)(.GlobalEnv)
      )

