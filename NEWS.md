# constructive 1.2.0

cynkra (https://cynkra.com) is officially a funder, already was from the start in practice, thanks cynkra!

Features and improvements:
 * We now support the "object" type, that many S7 objects are built on.
   The code to build an empty "object" is either `getClass("S4")@prototype |> asS3(complete = FALSE)`
   or `S7::S7_object() |> structure(class = NULL)`
 * We now support classes from the S7 package: 
   * "S7_Object"
   * "S7_union"
   * "S7_S3_class"
   * "S7_generic"
   * "S7_class"
   * "S7_base_class"
   * "S7_any"
 * We now support classes from the ellmer classes :
   * "ellmer::TypeObject"
   * "ellmer::TypeJsonSchema"
   * "ellmer::TypeEnum"
   * "ellmer::TypeBasic"
   * "ellmer::TypeArray"
* We now support the new ggplot2 classes:
  * "ggplot2::element_blank"
  * "ggplot2::element_geom"
  * "ggplot2::element_line"
  * "ggplot2::element_point"
  * "ggplot2::element_polygon"
  * "ggplot2::element_rect"
  * "ggplot2::element_text"
  * "ggplot2::ggplot"
  * "ggplot2::label"
  * "ggplot2::margin"
  * "ggplot2::theme"
  * "Guide"
  * "GuideAxisLogticks"
  * "GuideAxisStack"
  * "GuideAxisTheta"
  * "GuideBins"
  * "GuideColourBar"
  * "GuideColourSteps"
  * "GuideCustom"
  * "GuideLegend"
  * "GuideNone"
  * "Guides"
  * "labels"
  * "FacetGrid" (an old class that had been forgotten!)
* We don't signal a reconstruction inaccuracy when ggplot objects are reconstructed
  accurately except for stored language elements, e.g we consider `ggplot2::geom_point`
  and `geom_point` to be equivalent in stored calls. It's not fully satisfying because an aim
  of constructive is to spot such small details, but we also want to be practical
  and assessing the practical equivalence of the generated code seems more valuable
  than perfect accuracy there. Please open a ticket if this upsets you and we'll work something out.
* {cli} >= 3.1.0 is now required and `cli::code_highlight()` is now used, removing the dependency on {prettycode}. Turn it off with `options(constructive_pretty = FALSE)`.
  The `style` argument of `print.constructive_code()` is deprecated in favour of the new argument `code_theme`. Thanks @wurli!

Bug fixes:

* `deparse_call()` supports more corner cases (mostly for artificial cases you probably won't encounter)
* The construction of complex numbers now accounts for negative zeroes in imaginary and real parts
* Buggy arg forwarding in some internal functions was fixed
* The "list2env" constructor for environments was improved
* calls to the `function` function (of type "language") are correctly constructed
* We don't recycle with `data.frame()` when a column has attributes that don't support it
* S4 gains a "prototype" constructor, more general, that we fall back to by default where the class as an "initialize" method

# constructive 1.1.0

Features:

* Implemented construction of R6 objects
* Implemented construction of zoo and xts objects
* Implemented `"cbind"` and `"rbind"` constructors for matrices
* Now there's no need to namespace `opts_*()` functions if the package is not
  attached, this mean we can call for instance `constructive::construct(x, opts_tbl_df("tribble"))`
  rather than `constructive::construct(x, constructive::opts_tbl_df("tribble"))`
* New classes and constructors can now be implemented in your package without
  importing the package (adding to "Suggests" instead)
* Added a hex logo
  
Fixes and improvements:

* Fixed a display issue in `construct_diff()`
* `construct_clip()` now shows a console message in addition to writing to the
  clipboard
* Improved support of data frames, tibbles, and data.tables regarding non syntactic, empty,
`NA` or duplicate names
* Improved recycling in data frames
* Implemented support for dates in `tribble()` calls when calling `construct(x, opts_tbl_df("tribble"))`
* Improved support of classed matrices
* Fixed timestamp precision issue
* Improved support of `"POSIXlt"` class
* Improved encoding support
* Fixed construction of vectors with `numeric_version()`, `package_version()`,
  and `R_system_version()`
* Fixed construction of edge case calls like ``(`fun<-`)()`` or ``(`a b`)()``
* Fixed construction of environments with `NULL` parents

# constructive 1.0.1

* We fixed a typo that was breaking `.cstr_new_class(, commented = TRUE)`
* We fixed a bug regarding high precedence infix operators that was breaking `deparse_call()`
* "simpleUnit" is considered as a "grid" package class, not a "base" class and
  we fixed the check for corrupted "simpleUnit" objects
* The construction of "ScalesList" objects is improved, it failed in some cases
* The construction of "CoordCartesian" is improved and considers the "default" argument
* Improved vignette consistency

# constructive 1.0.0

## Internals, extension system, cosmetics

* The internals have changed quite a bit, and a new extension system has been
developed.
* The vignette "extend-constructive" details it and how constructive works.
* The package {constructive.example} was updated to reflect the new system.
* We remove non API calls in the C code, to comply with new CRAN rules.
* The pkgdown website categorizes the functions so the package can be explored
  more conveniently (Thanks Maelle Salmon @maelle for the suggestion).
* The doc was improved overall
* The README was reworked
  
## New functions

* Two new functions `construct_dput()` and `construct_base()` allow to construct
  objects respectively without using high level constructors at all, or using
  only those included in base packages (such as `data.frame()` etc).
* A new function `construct_clip()` is just like `construct()` but 
  copies code directly to the clipboard (Thanks Josiah Parry @JosiahParry for the suggestion).
* We now have `opts_logical()`, `opts_integer()`, `opts_double()`, `opts_complex()`,
  `opts_character()` and `opts_raw()` so options can be set independently. 
  `opts_atomic()` still works to set a. behavior for all atomics but the new functions have precedence.
  The "raw" type is supported better can be constructed from integers in
  decimal or hexadecimal notation with the "as.raw" constructor, or from 
  character using the "charToRaw" constructor.
* The new functions `.cstr_new_class()` and `.cstr_new_template()`
  facilitate the process of working with the new extension system.

## New classes

* Expression vectors are now supported (For some reason we had missed it!).
* We implement constructors for the following base R classes:
  * citationFooter
  * citationHeader
  * hexmode
  * octmode
  * person
  * difftime
  * simpleError
  * simpleWarning
  * simpleMessage
  * simpleCondition
  * errorCondition
  * warningCondition
* We support the class "integer64" from the {bit64} package. It was important 
  because we can't recreate `NA` or negative integer64 objects using base R only.
* We support the class "blob" from the {blob} package.

## New features and deprecation

* Many constructive functions gain the `classes` argument that generalize
  `construct_dput()` and `construct_base()`, so users can enable or disable the idiomatic construction of some
  classes.
* `construct_reprex()` and `construct_multi()` gain the `include_dotted = TRUE`
  argument so we can optionally disable the construction of objects such as
  `.Random.seed` int he global environment or `.Class` in the execution environment
  of S3 methods.s
* `construct()` and `construct_multi()` gain the arguments `unicode_representation` 
  and `escape` previously used by `opts_atomic()` and these are now not only 
  applied on strings but also on element names and variable names.
* We look at the encoding when constructing character vectors, so an UTF-8
  "é" is not constructed like a latin-1 "é" anymore. Hopefully this will help
  some users out of encoding hell faster.
* `opts_tbl_df()` gains a `justify` argument to control the justification of
  columns with `constructor = `"tribble"` (Thanks Jacob Scott @wurli for the implementation).
* The imports and lazydata environments of packages are constructed with
 `parent.env(asNamespace("pkg"))` and `getNamespaceInfo("pkg", "lazydata")`.
 Before that they were constructed as regular environments.
* When constructing environments we now lock environments and bindings when relevant.
* We construct negative zeroes as `-0`. `identical(0, -0))` is `TRUE` but
  `1/-0` is `-Inf` so it made sense to support them.
* `opts_environment()` gains a `"predefine"` constructor and 
  `opts_environment(predefine = TRUE)` is deprecated. The old way still works
  but warns and is not documented anymore.
* In `opts_atomic()` the arguments `unicode_representation` and `escape` are
  deprecated, use the new `opts_character()` function or set them in the 
  main function directly instead so they also affect symbols and argument names.
  The old way still works but warns and is not documented anymore.

## Fixes

* We solve some operator precedence issues in `deparse_call()`
* Named vectors of length 1 are constructed properly
* Objects are constructed properly if their names have attributes, contain `NA`s,
  or are named like `c()`'s arguments `recursive` and `use.names`
* Circularity is detected when attempting to construct an environment refering to
  itself with an inappropriate constructor. It now fails explicitly rather than
  trigger an infinite loop or a low level error.
* `NA`s and `NaN`s are not conflated anymore when compressing double vectors 
* complex numbers are constructed properly regarding the different combination
  of `NA` values in their real and imaginary parts. 
* We check for the S4 bit using `isS4()` and use `asS4()` when necessary
* In `opts_numeric_version()`, `opts_package_version()` and `opts_R_system_version()`
the incorrectly named "atomic" constructor is replaced by a "list" constructor
* data frames, data tables and tibbles are now properly constructed when they
  contain columns that their idiomatic constructors cannot handle, such
  as "row.names", or "stringsAsFactors" for data frames.
* POSIXlt are constructed according to the R version, to account for the changes
  in R 4.3.0
* We can construct objects with a ".Data" attribute, this used to fail because
  `structure()` has a `.Data` argument.
* The default "row.names" attribute is built with the `c(NA, -<nrow>)` form,
  as in `dput()`, this solves some rare corner cases.
* We fix some issues with raw strings when constructing character vectors
* We fix some issues with `NA`, `NaN`, `Inf` dates and `NULL` timezones
* We fix the environment attribute repair
* Functions with non syntactic formals are constructed properly

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
* `deparse_call()` is more robust and gains the arguments `escape` and `unicode_representation`
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
