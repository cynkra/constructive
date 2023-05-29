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
