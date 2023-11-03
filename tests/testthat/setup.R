colon_colon <- `::`

`::` <- function(x, y) {
  x_sym <- ensym(x)
  y_sym <- ensym(y)
  tryCatch(
    inject(colon_colon(!!x_sym, !!y_sym)),
    packageNotFoundError = function(e) {
      skip_if_not_installed(as_string(x_sym))
    }
  )
}
