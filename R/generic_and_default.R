construct_raw <- function(x, ..., data = NULL) {
  idiomatic_code <- data_match(x, data) %||% construct_idiomatic(x, ..., data = data)
  repaired_code <- repair_attributes(x, idiomatic_code, ..., data = data)
  repaired_code
}

data_match <- function(x, data) {
  if (is.null(data)) return(NULL)
  m <- match2(x, data)
  if (!length(m)) return(NULL)
  names(data)[m]
}






