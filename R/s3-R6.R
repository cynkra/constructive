#' Constructive options for class 'R6'
#'
#' These options will be used on objects of class 'R6'.
#'
#' Depending on `constructor`, we construct the object as follows:
#'
#' * `"R6Class"` (default): We build the object using `R6Class()$new()`, see details.
#' * `"next"` : Use the constructor for the next supported class.
#'
#' Objects of class "R6" are harder to construct than "R6ClassGenerator" objects,
#' because  'constructive' doesn't know by default the constructor (i.e. class
#' generator) that was used to build them. So what we do is we build a class
#' generator that generates this object by default. This is why the generated code
#' is in the form `R6Class()$new()`.
#'
#' Another layer of complexity is added when the object features an
#' `initialize()` method, we cannot implement it in the class generator because
#' it might change the behavior of `$new()` and return a wrong result (or fail).
#' For this reason the `initialize()` method when it exists is repaired as an
#' extra step.
#'
#' `construct_diff()` works well to inspect the differences between two R6
#' objects where alternatives like `waldo::compare()` or `base::all.equal()`
#' don't return anything informative.
#'
#' @param constructor String. Name of the function used to construct the object.
#' @param ... Additional options used by user defined constructors through the `opts` object
#' @return An object of class <constructive_options/constructive_options_R6>
#' @export
opts_R6 <- function(constructor = c("R6Class", "next"), ...) {
  constructive::.cstr_options("R6", constructor = constructor[[1]], ...)
}

#' @exportS3Method constructive::.cstr_construct
.cstr_construct.R6 <- function(x, ...) {
  opts <- list(...)$opts$R6 %||% opts_R6()
  if (is_corrupted_R6(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.R6", structure(NA, class = opts$constructor))
}

is_corrupted_R6 <- function(x) {
  FALSE
}

#' @export
.cstr_construct.R6.R6Class <- function(x, ...) {
  x_has_initialize <- !is.null(x$initialize)
  public <- as.list.environment(x, all.names = TRUE)
  public$clone <- NULL
  public$.__enclos_env__ <- NULL
  if (x_has_initialize) public["initialize"] <- list(NULL)
  if (is.null(x$.__enclos_env__$private)) {
    private <- NULL
  } else {
    private <- as.list.environment(x$.__enclos_env__$private, all.names = TRUE)
  }

  classname <- class(x)[[which(class(x) == "R6") - 1]]
  args <- list(
    classname = classname,
    public = public,
    private = private,
    parent_env = parent.env(x)
  )

  code <- constructive::.cstr_apply(args, fun = "R6::R6Class", ...)
  code[[length(code)]] <- paste0(code[[length(code)]], "$new()")

  if (x_has_initialize) {
    init_code <- .cstr_construct(x$initialize, ...)
    init_code[[length(init_code)]] <- paste0(init_code[[length(init_code)]], ",")
    if (list(...)$one_liner) {
      init_code <- sprintf("(function(r6_obj) {assign(\"initialize\", %s envir = r6_obj); r6_obj})()", init_code)
    } else {
      init_code <- c("assign(", "  'initialize',", paste0("  ", init_code), "  envir = r6_obj", ")")
      init_code <- c(
        "(function(r6_obj) {",
        paste0("  ", init_code),
        "r6_obj",
        "})()")
    }

    code <- .cstr_pipe(code, init_code, ...)
  }

  constructive::.cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = c(classname, "R6")
  )
}
