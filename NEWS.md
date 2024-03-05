# constructive 0.3.0

* A new debugging tool, the `construct_reprex()` function, is introduced. 
  It can be called in any function and will construct all variables and arguments
  where it's called.
* `construct_multi()` now constructs promises, in practice this is useful so
  we can construct the evaluation environment of a function, including the uneavaluated
  and potentially NSE args of a function
* Set `options(constructive_print_mode = <character>)` where `<character>` is a vector
    of strings among `"console"`, `script`, `"clipboard"` and `"reprex"`. The default
    behavior is "console". See `` ?`constructive-global_options`
* `opts_atomic(escape = FALSE)` (the default) now correctly uses surrounding single quotes
  for strings that contain double quotes and no single quotes.
* `deparse_call()` is more robusts and gains the arguments `escape` and `unicode_representation`
  that were already present in `opts_atomic()`
* The ggplot object generation supports the internal changes of ggplot2 3.5.0,
  and the resulting construction is nicer.
* Data frames can be reconstructed when their columns don't have a `data.frame()`
  method
* The "read.table" constructor for data frames supports the `one_liner` argument
* roxygen2 is Suggested (not Imported anymore), the ellipsis dependency is removed
* formulas have a "next" constructor, useful to see what formulas are at a low
  level
* classes with S3 methods for `length`, `[`, `[[` etc are handled better
* `.env()` doesn't crash anymore when provided a wrong or obsolete memory address
* Integer vectors are constructed properly when they feature consecutive elements
  differing by more than `.Machine$integer.max` 
* Classed objects of types "..." and "externalptr" and are constructed properly
* S4 construction uses the `slot` argument rather than the `representation` arg

# constructive 0.2.0

* We don't use {styler} anymore, performance is sensibly enhanced as
a consequence.
* The package works without having {prettycode} installed, but uses it if it's installed
* We default to using the magrittr pipe `%>%` for R versions that don't support `|>`
* `NA` levels are supported for the classes "factor" and "ordered"
* Environment construction includes variables prefixed with a dot
* When we don't attach the package, we don't need to use `constructive::` in the
  `...` before `opts_*` functions, for instance we can call `constructive::construct(cars, opts_data.frame("read.table"))`.
* `quote({})` is now constructed as `"{ }"` rather than ``"`{`()"``

# constructive 0.1.0

* First CRAN release
* `construct()` generates the code to build an object using idiomatic code, it
  wraps the lower level `.cstr_construct()` S3 generic.
* We currently support 66 classes/types: "array", "AsIs", "atomic", "classGeneratorFunction", 
  "classPrototypeDef", "classRepresentation", "constructive_options", "CoordCartesian",
  "CoordFixed", "CoordFlip", "CoordMap", "CoordMunch", "CoordPolar", "CoordQuickmap",
  "CoordSf", "CoordTrans", "data.frame", "data.table", "Date", "default", "dm", 
  "dots", "element_blank", "element_grob", "element_line", "element_rect", 
  "element_render", "element_text", "environment", "externalptr", "FacetWrap", 
  "factor", "formula", "function", "ggplot", "ggproto", "grouped_df", "labels", 
  "language", "Layer", "list", "margin", "matrix", "mts", "numeric_version", 
  "ordered", "package_version", "pairlist", "POSIXct", "POSIXlt", "quosure", 
  "quosures", "R_system_version", "rel", "rowwise_df", "S4", "Scale", "ScalesList", 
  "simpleUnit", "tbl_df", "theme", "ts", "uneval", "vctrs_list_of", "waiver" and 
  "weakref".
* A set of functions prefixed with `opts_` can be used to choose various constructors
  and apply parameters to tweak the output.
* A special constructor named "next" can be used to fall back on the next 
  `.cstr_construct()` method. This is useful to explore objects at a level one 
  step lower than the idiomatic constructor.
* When a corrupted object is encountered the next method is used.
* Users can extend the package using a set of exported functions prefixed with `.cstr_`,
  a vignette describes how to proceed.
* `construct_issues()` is used without arguments to check what were the issues encountered
  with the last reconstructed object, it can also be provided a specific constructive object.
* `construct_diff()` highlights the differences in the code used to produce 2 objects.
* `construct_multi()` constructs several objects from a named list,
* `construct_dump()` is similar to `base::dump()`, it's a wrapper around `construct_multi()`
  that writes to a file.
* `construct_signature()` constructs a function signature such as the one we see in the
  "usage" section of a function's help file.
outputs the code produced  
* `deparse_call()` is an alternative to `base::deparse()` and `rlang::expr_deparse()` that 
  handles additional corner cases and fails when encountering tokens other than symbols 
  and syntactic literals .

# constructive 0.0.1

* {constructive} produces code that can be used to recreate R objects. In a sense it
is similar to `base::dput()` or `base::deparse()` but {constructive} strives to use "natural" constructors
(`factor` for factors, `as.Date()` for dates, `data.frame()` for data frames etc),
in order to get output readable by humans.
