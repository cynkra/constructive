# construct_serialize(c("ab", "aé"))
construct_serialize <- function(x) {
  # 1. Convert input object to a raw vector
  raw_vector <- serialize(x, connection = NULL, ascii = FALSE)

  # 2. Process the header part of the raw vector
  header_res <- serialize_header(raw_vector)

  # 3. Process the data part of the raw vector
  data_res <- serialize_data(header_res$x, header_res$i)

  # 4. Trim the final trailing comma from each block of code
  # (trim_last_comma is defined in utils.R)
  header_code <- trim_last_comma(header_res$code)
  data_code <- trim_last_comma(data_res$code)

  # 5. Combine the generated code parts, matching the example style
  code <- c(
    "# --- HEADER ---",
    "c(",
    paste0("  ", header_code),
    "),",
    "# --- DATA ---",
    "c(",
    paste0("  ", data_code),
    ")"
  )

  # 6. Format the code into a single string and wrap it
  code <- c(
    "unserialize(as.raw(c(",
    paste0("  ", code),
    ")))"
  )
  code <- structure(code, class = "constructive_code")
  return(code)
}
