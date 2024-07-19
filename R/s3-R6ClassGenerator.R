#' Constructive options for class 'R6ClassGenerator'
#'
#' These options will be used on objects of class 'R6ClassGenerator'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"R6Class"` (default): We build the object using `R6Class()`.
#' * `"next"` : Use the constructor for the next supported class.
#'
#' @param constructor String. Name of the function used to construct the object.
#' @param ... Additional options used by user defined constructors through the `opts` object
#' @return An object of class <constructive_options/constructive_options_R6ClassGenerator>
#' @export
opts_R6ClassGenerator <- function(constructor = c("R6Class", "next"), ...) {
  constructive::.cstr_options("R6ClassGenerator", constructor = constructor[[1]], ...)
}

#' @exportS3Method constructive::.cstr_construct
.cstr_construct.R6ClassGenerator <- function(x, ...) {
  opts <- list(...)$opts$R6ClassGenerator %||% opts_R6ClassGenerator()
  if (is_corrupted_R6ClassGenerator(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.R6ClassGenerator", structure(NA, class = opts$constructor))
}

is_corrupted_R6ClassGenerator <- function(x) {
  FALSE
}

#' @export
.cstr_construct.R6ClassGenerator.R6Class <- function(x, ...) {
  public_methods <- x$public_methods
  public_methods$clone <- NULL
  # FIXME: this might be a feature for the function constructor
  # but I think we could recognize a function from its package and use pkg::fun or pkg:::fun
  args <- list(
    x$classname,
    public = c(x$public_fields, public_methods),
    private = c(x$private_fields, x$private_methods),
    active = x$active,
    inherit = x$inherit,
    lock_objects = x$lock_objects,
    class = x$class,
    portable = x$portable,
    lock_class = x$lock_class,
    cloneable = x$cloneable,
    parent_env = x$parent_env
  )
  # remove args equal to default
  if (is.null(args$classname)) args$classname <- NULL
  if (!length(args$public)) args$public <- NULL
  if (!length(args$private)) args$private <- NULL
  if (!length(args$active)) args$active <- NULL
  if (!length(args$inherit)) args$inherit <- NULL
  if (isTRUE(args$lock_objects)) args$lock_objects <- NULL
  if (isTRUE(args$class)) args$class <- NULL
  if (isTRUE(args$portable)) args$portable <- NULL
  if (isFALSE(args$lock_class)) args$lock_class <- NULL
  if (isTRUE(args$cloneable)) args$cloneable <- NULL
  code <- constructive::.cstr_apply(args, fun = "R6::R6Class", ...)
  constructive::.cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = "R6ClassGenerator"
  )
}

compare_proxy_R6ClassGenerator <- function(x, path) {
  # env_clone() doesn't do deep cloning and waldo::compare() sees as different
  # 2 equivalent env if they are named, so we need this trick
  parent <- env_clone(parent.env(x))
  attr(parent, "name") <- NULL
  environment(parent$all_named) <- parent
  environment(parent$assign_func_envs) <- parent
  environment(parent$create_super_env) <- parent
  environment(parent$get_functions) <- parent
  environment(parent$get_nonfunctions) <- parent
  environment(parent$get_superclassnames) <- parent
  environment(parent$list2env2) <- parent
  environment(parent$merge_vectors) <- parent

  x <- rlang::env_clone(x, parent = parent)
  attr(x, "name") <- NULL
  environment(x$undebug) <- x
  environment(x$clone_method) <- x
  environment(x$new) <- x
  environment(x$is_locked) <- x
  environment(x$lock) <- x
  environment(x$unlock) <- x
  environment(x$has_private) <- x
  environment(x$set) <- x
  environment(x$public_methods$clone) <- x
  environment(x$get_inherit) <- x
  environment(x$debug) <- x
  x$self <- x

  list(object = x, path = path)
}
