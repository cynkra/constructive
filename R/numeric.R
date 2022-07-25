
# this is necessary because `dput()`, used for default the method, sometimes cuts values too short,
# however this gives ugly values in the general case
# in the 2 following cases we want the shortest "equal" output
# format(5.1, digits = 22) # "5.099999999999999644729"
# format(1e24, digits = 22) # 999999999999999983222784

# => so we use `digits = 16` for the default since it seems to simplify those values,
#    and we fall back on `digits = 22` for other cases

# It doesn't fix everything, but this might be because of a system dependent bug:
# In the following case it's worse, the output is NOT equal to the input
# format(1e25, digits = 22) # 999999999999999983222784
# we want to use only as many digits as necessary

# all the above for :
# R version 4.1.3 (2022-03-10)
# Platform: aarch64-apple-darwin20 (64-bit)
# Running under: macOS Monterey 12.0.1

#' @export
construct_idiomatic.double <- function(x, max_atomic = NULL, ...) {
  if (length(x) == 0 || (!is.null(max_atomic) && max_atomic == 0)) return("numeric(0)")

  format_flex <- function(x) {
    formatted <- format(x, digits = 16)
    if (as.numeric(formatted) == x) return(formatted)
    format(x, digits = 22)
  }
  if (length(x) == 1 && is.null(names(x))) return(format_flex(x))

  if (!is.null(max_atomic) && length(x) > max_atomic) {
    x <- x[seq_len(max_atomic)]
    code <- construct_apply(vapply(x, format_flex, character(1)), "c", new_line = FALSE, language = TRUE, ...)
    code[[length(code)]] <- sub(")$", ", ...)", code[[length(code)]])
    return(code)
  }
  construct_apply(vapply(x, format_flex, character(1)), "c", new_line = FALSE, language = TRUE, ...)
}

