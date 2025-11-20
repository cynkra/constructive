#' Build code to recreate an object from its serialized form
#'
#' `construct_serialize()` creates code that reconstructs an R object by serializing
#' it to raw bytes and then calling `unserialize()`. This is a fallback method that
#' works for almost any R object, including those that are difficult or impossible
#' to construct idiomatically.
#'
#' @details
#'
#' This function generates code in the form:
#' ```r
#' unserialize(as.raw(c(
#'   # --- HEADER ---
#'   c(0x58, 0x0a, ...),
#'   # --- DATA ---
#'   c(0x00, 0x00, ...)
#' )))
#' ```
#'
#' The generated code includes detailed comments explaining the binary structure,
#' making it useful for understanding R's serialization format.
#'
#' ## When to use construct_serialize()
#'
#' Use `construct_serialize()` when:
#' - You need a guaranteed way to reconstruct any R object
#' - Idiomatic construction is unavailable or produces incorrect results
#' - You want to understand R's serialization format
#' - You need to preserve exact binary representation (e.g., special NA values)
#'
#' Prefer `construct()` when:
#' - You want readable, idiomatic R code
#' - The object has a natural constructor (e.g., `data.frame()`, `matrix()`)
#' - You're sharing code with others who need to understand it
#'
#' ## Supported Types
#'
#' `construct_serialize()` supports all commonly used R object types:
#' - **Atomic vectors**: character, logical, integer, numeric, complex, raw
#' - **Special values**: NULL, NA, NaN, Inf, -Inf, -0 (negative zero)
#' - **Containers**: lists, pairlists, data.frames
#' - **Attributes**: names, class, dim, dimnames, custom attributes
#' - **Functions**: closures, builtins (sum, length), special forms (if, for)
#' - **Expressions**: symbols, language objects (calls), expression vectors
#' - **Environments**: global environment references, custom environments
#' - **Advanced**: ALTREP sequences (1:n), references, factors, dates, formulas
#'
#' ## Limitations
#'
#' Some R objects cannot be fully serialized:
#' - **External pointers**: Cannot be reconstructed in a new session
#' - **Namespace environments**: May reference package-specific state
#' - **Active bindings**: The binding mechanism isn't preserved
#' - **Connections**: File handles and network connections
#' - **Promises**: Unevaluated function arguments
#'
#' For these cases, `unserialize()` may return a placeholder or fail.
#'
#' @param x An R object to serialize and reconstruct.
#'
#' @return An object of class `"constructive_code"` containing the generated code
#'   as a character vector. Each element is one line of code.
#'
#' @seealso [construct()] for idiomatic object construction, [serialize()] for
#'   the underlying serialization mechanism.
#'
#' @export
#' @examples
#' # Simple objects
#' construct_serialize(1:5)
#' construct_serialize(c("hello", "world"))
#'
#' # Complex objects
#' construct_serialize(data.frame(a = 1:3, b = letters[1:3]))
#' construct_serialize(matrix(1:6, nrow = 2))
#'
#' # Objects with attributes
#' x <- c(a = 1, b = 2, c = 3)
#' construct_serialize(x)
#'
#' # Expressions and calls
#' construct_serialize(quote(mean(x)))
#' construct_serialize(expression(x + 1, y * 2))
#'
#' # Special values
#' construct_serialize(c(NA, NaN, Inf, -Inf))
#'
#' # Builtin functions
#' construct_serialize(sum)
#' construct_serialize(`if`)
#'
#' # The generated code can be evaluated to reconstruct the object
#' code <- construct_serialize(iris)
#' reconstructed <- eval(parse(text = paste(code, collapse = "\n")))
#' identical(reconstructed, iris)
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
