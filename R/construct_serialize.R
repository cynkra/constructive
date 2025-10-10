# construct_serialize(c("ab", "aé"))
construct_serialize <- function(x) {
  # 1. Convert input object to a raw vector
  raw_vector <- serialize(x, connection = NULL, ascii = FALSE)
  
  # 2. Process the header part of the raw vector
  header_res <- serialize_header(raw_vector)
  
  # 3. Process the data part of the raw vector
  data_res <- serialize_data(header_res$x, header_res$i)
  
  # 4. Combine the generated code parts, matching the example style
  code <- c(
    "# --- HEADER ---",
    "c(",
    paste0("  ", header_res$code),
    "),",
    "# --- DATA ---",
    "c(",
    paste0("  ", data_res$code),
    ")"
  )
  
  # 5. Format the code into a single string and wrap it
  code <- c(
    "unserialize(as.raw(c(",
    paste0("  ", code),
    ")))"
  )
  code <- structure(code, class = "constructive_code")
  return(code)
}
