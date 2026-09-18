# Build code to recreate an object

- `construct()` builds the code to reproduce one object,

- `construct_multi()` builds the code to reproduce objects stored in a
  named list or environment.

## Usage

``` r
construct(
  x,
  ...,
  data = NULL,
  pipe = NULL,
  check = NULL,
  unicode_representation = c("ascii", "latin", "character", "unicode"),
  escape = FALSE,
  pedantic_encoding = FALSE,
  compare = compare_options(),
  one_liner = FALSE,
  template = getOption("constructive_opts_template"),
  classes = NULL
)

construct_multi(
  x,
  ...,
  data = NULL,
  pipe = NULL,
  check = NULL,
  unicode_representation = c("ascii", "latin", "character", "unicode"),
  escape = FALSE,
  pedantic_encoding = FALSE,
  compare = compare_options(),
  one_liner = FALSE,
  template = getOption("constructive_opts_template"),
  classes = NULL,
  include_dotted = TRUE
)
```

## Arguments

- x:

  An object, for `construct_multi()` a named list or an environment.

- ...:

  Constructive options built with the `opts_*()` family of functions.
  See the "Constructive options" section below.

- data:

  Named list or environment of objects we want to detect and mention by
  name (as opposed to deparsing them further). Can also contain unnamed
  nested lists, environments, or package names, in the latter case
  package exports and datasets will be considered. In case of conflict,
  the last provided name is considered.

- pipe:

  Which pipe to use, either `"base"` or `"magrittr"`. Defaults to
  `"base"` for R \>= 4.2, otherwise to `"magrittr"`.

- check:

  Boolean. Whether to check if the created code reproduces the object
  using
  [`waldo::compare()`](https://waldo.r-lib.org/reference/compare.html).

- unicode_representation:

  By default "ascii", which means only ASCII characters (code point
  \< 128) will be used to construct strings and variable names. This
  makes sure that homoglyphs (different spaces and other identically
  displayed unicode characters) are printed differently, and avoid
  possible unfortunate copy and paste auto conversion issues. "latin" is
  more lax and uses all latin characters (code point \< 256).
  "character" shows all characters, but not emojis. Finally "unicode"
  displays all characters and emojis, which is what
  [`dput()`](https://rdrr.io/r/base/dput.html) does.

- escape:

  Boolean. Whether to escape double quotes and backslashes. If `FALSE`
  we use single quotes to surround strings (including variable and
  element names) containing double quotes, and raw strings for strings
  that contain backslashes and/or a combination of single and double
  quotes. Depending on `unicode_representation` `escape = FALSE` cannot
  be applied on all strings.

- pedantic_encoding:

  Boolean. Whether to mark strings with the "unknown" encoding rather
  than an explicit native encoding ("UTF-8" or "latin1") when it's
  necessary to reproduce the binary representation exactly. This detail
  is normally of very little significance. The reason why we're not
  pedantic by default is that the constructed code might be different in
  the console and in snapshot tests and reprexes due to the latter
  rounding some angles, and it would be confusing for users.

- compare:

  Parameters passed to
  [`waldo::compare()`](https://waldo.r-lib.org/reference/compare.html),
  built with
  [`compare_options()`](https://cynkra.github.io/constructive/reference/compare_options.md).

- one_liner:

  Boolean. Whether to collapse the output to a single line of code.

- template:

  A list of constructive options built with `opts_*()` functions, they
  will be overriden by `...`. Use it to set a default behavior for
  `{constructive}`.

- classes:

  A character vector of classes for which to use idiomatic constructors
  when available, we can provide a package instead of all its classes,
  in the "{pkg}" form, and we can use a minus sign (inside the quotes)
  to exclude rather than include. By default we use idiomatic
  constructors whenever possible. The special values `"*none*"` and
  `"*base*"` can be used to restrict the idiomatic construction to the
  objects. See
  [`construct_dput()`](https://cynkra.github.io/constructive/reference/construct_dput.md)
  and
  [`construct_base()`](https://cynkra.github.io/constructive/reference/construct_dput.md)
  for wrappers around this feature.

- include_dotted:

  Whether to include names starting with dots, this includes
  `.Random.seed` in the global environment and objects like `.Class` and
  `.Generic` in the execution environments of S3 methods.

## Value

An object of class 'constructive'.

## Details

`construct_multi()` recognizes promises (also called lazy bindings),
this means that for instance `construct_multi(environment())` can be
called when debugging a function and will construct unevaluated
arguments using
[`delayedAssign()`](https://rdrr.io/r/base/delayedAssign.html).

## See also

[`construct_dput()`](https://cynkra.github.io/constructive/reference/construct_dput.md)
[`construct_base()`](https://cynkra.github.io/constructive/reference/construct_dput.md)
[`construct_clip()`](https://cynkra.github.io/constructive/reference/construct_clip.md)
[`construct_dump()`](https://cynkra.github.io/constructive/reference/construct_dump.md)
[`construct_reprex()`](https://cynkra.github.io/constructive/reference/construct_reprex.md)
[`construct_diff()`](https://cynkra.github.io/constructive/reference/construct_diff.md)

## Constructive options

Constructive options provide a way to customize the output of
\`construct()\`. We can provide calls to \`opts\_\*()\` functions to the
\`...\` argument. Each of these functions targets a specific type or
class and is documented on its own page.

- [`opts_array`](https://cynkra.github.io/constructive/reference/opts_array.md)`(constructor = c("array", "next"), ...)`

- [`opts_AsIs`](https://cynkra.github.io/constructive/reference/opts_AsIs.md)`(constructor = c("I", "next"), ...)`

- [`opts_atomic`](https://cynkra.github.io/constructive/reference/opts_atomic.md)`(..., trim = NULL, fill = c("default", "rlang", "+", "...", "none"), compress = TRUE)`

- [`opts_bibentry`](https://cynkra.github.io/constructive/reference/other-opts.md)`(constructor = c("bibentry", "next"), ...)`

- [`opts_blob`](https://cynkra.github.io/constructive/reference/opts_blob.md)`(constructor = c("blob", "next"), ...)`

- [`opts_character`](https://cynkra.github.io/constructive/reference/opts_character.md)`(constructor = c("default"), ..., trim = NULL, fill = c("default", "rlang", "+", "...", "none"), compress = TRUE, unicode_representation = c("ascii", "latin", "character", "unicode"), escape = FALSE)`

- [`opts_citationFooter`](https://cynkra.github.io/constructive/reference/other-opts.md)`(constructor = c("citFooter", "next"), ...)`

- [`opts_citationHeader`](https://cynkra.github.io/constructive/reference/other-opts.md)`(constructor = c("citHeader", "next"), ...)`

- [`opts_classGeneratorFunction`](https://cynkra.github.io/constructive/reference/opts_classGeneratorFunction.md)`(constructor = c("setClass"), ...)`

- [`opts_classPrototypeDef`](https://cynkra.github.io/constructive/reference/opts_classPrototypeDef.md)`(constructor = c("prototype"), ...)`

- [`opts_classRepresentation`](https://cynkra.github.io/constructive/reference/opts_classRepresentation.md)`(constructor = c("getClassDef"), ...)`

- [`opts_complex`](https://cynkra.github.io/constructive/reference/opts_complex.md)`(constructor = c("default"), ..., trim = NULL, fill = c("default", "rlang", "+", "...", "none"), compress = TRUE)`

- [`opts_constructive_options`](https://cynkra.github.io/constructive/reference/opts_constructive_options.md)`(constructor = c("opts", "next"), ...)`

- [`opts_CoordCartesian`](https://cynkra.github.io/constructive/reference/other-opts.md)`(constructor = c("default", "coord_cartesian", "next", "environment"), ...)`

- [`opts_CoordFixed`](https://cynkra.github.io/constructive/reference/other-opts.md)`(constructor = c("coord_fixed", "next", "environment"), ...)`

- [`opts_CoordFlip`](https://cynkra.github.io/constructive/reference/other-opts.md)`(constructor = c("coord_flip", "next", "environment"), ...)`

- [`opts_CoordMap`](https://cynkra.github.io/constructive/reference/other-opts.md)`(constructor = c("coord_map", "next", "environment"), ...)`

- [`opts_CoordMunch`](https://cynkra.github.io/constructive/reference/other-opts.md)`(constructor = c("coord_munch", "next", "environment"), ...)`

- [`opts_CoordPolar`](https://cynkra.github.io/constructive/reference/other-opts.md)`(constructor = c("coord_polar", "next", "environment"), ...)`

- [`opts_CoordQuickmap`](https://cynkra.github.io/constructive/reference/other-opts.md)`(constructor = c("coord_quickmap", "next", "environment"), ...)`

- [`opts_CoordRadial`](https://cynkra.github.io/constructive/reference/opts_CoordRadial.md)`(constructor = c("coord_radial", "next"), ...)`

- [`opts_CoordSf`](https://cynkra.github.io/constructive/reference/other-opts.md)`(constructor = c("coord_sf", "next", "environment"), ...)`

- [`opts_CoordTrans`](https://cynkra.github.io/constructive/reference/other-opts.md)`(constructor = c("coord_trans", "next", "environment"), ...)`

- [`opts_CoordTransform`](https://cynkra.github.io/constructive/reference/opts_CoordTransform.md)`(constructor = c("coord_transform", "next"), ...)`

- [`opts_data.frame`](https://cynkra.github.io/constructive/reference/opts_data.frame.md)`(constructor = c("data.frame", "read.table", "next", "list"), ..., recycle = TRUE)`

- [`opts_data.table`](https://cynkra.github.io/constructive/reference/opts_data.table.md)`(constructor = c("data.table", "next", "list"), ..., selfref = FALSE, recycle = TRUE)`

- [`opts_Date`](https://cynkra.github.io/constructive/reference/opts_Date.md)`(constructor = c("as.Date", "as_date", "date", "new_date", "as.Date.numeric", "as_date.numeric", "next", "double"), ..., origin = "1970-01-01")`

- [`opts_difftime`](https://cynkra.github.io/constructive/reference/other-opts.md)`(constructor = c("as.difftime", "next"), ...)`

- [`opts_dm`](https://cynkra.github.io/constructive/reference/opts_dm.md)`(constructor = c("dm", "next", "list"), ...)`

- [`opts_dots`](https://cynkra.github.io/constructive/reference/opts_dots.md)`(constructor = c("default"), ...)`

- [`opts_double`](https://cynkra.github.io/constructive/reference/opts_double.md)`(constructor = c("default"), ..., trim = NULL, fill = c("default", "rlang", "+", "...", "none"), compress = TRUE)`

- [`opts_element_blank`](https://cynkra.github.io/constructive/reference/other-opts.md)`(constructor = c("element_blank", "next", "list"), ...)`

- [`opts_element_grob`](https://cynkra.github.io/constructive/reference/other-opts.md)`(constructor = c("element_grob", "next", "list"), ...)`

- [`opts_element_line`](https://cynkra.github.io/constructive/reference/other-opts.md)`(constructor = c("element_line", "next", "list"), ...)`

- [`opts_element_rect`](https://cynkra.github.io/constructive/reference/other-opts.md)`(constructor = c("element_rect", "next", "list"), ...)`

- [`opts_element_render`](https://cynkra.github.io/constructive/reference/other-opts.md)`(constructor = c("element_render", "next", "list"), ...)`

- [`opts_element_text`](https://cynkra.github.io/constructive/reference/other-opts.md)`(constructor = c("element_text", "next", "list"), ...)`

- [`opts_ellmer_TypeArray`](https://cynkra.github.io/constructive/reference/opts_ellmer_TypeArray.md)`(constructor = c("type_array", "TypeArray", "next"), ...)`

- [`opts_ellmer_TypeBasic`](https://cynkra.github.io/constructive/reference/opts_ellmer_TypeBasic.md)`(constructor = c("default", "TypeBasic", "next"), ...)`

- [`opts_ellmer_TypeEnum`](https://cynkra.github.io/constructive/reference/opts_ellmer_TypeEnum.md)`(constructor = c("type_enum", "TypeEnum", "next"), ...)`

- [`opts_ellmer_TypeJsonSchema`](https://cynkra.github.io/constructive/reference/opts_ellmer_TypeJsonSchema.md)`(constructor = c("type_from_schema", "TypeJsonSchema", "next"), ...)`

- [`opts_ellmer_TypeObject`](https://cynkra.github.io/constructive/reference/opts_ellmer_TypeObject.md)`(constructor = c("type_object", "TypeObject", "next"), ...)`

- [`opts_environment`](https://cynkra.github.io/constructive/reference/opts_environment.md)`(constructor = c(".env", "list2env", "as.environment", "new.env", "topenv", "new_environment", "predefine"), ..., recurse = FALSE)`

- [`opts_error`](https://cynkra.github.io/constructive/reference/other-opts.md)`(constructor = c("errorCondition", "next"), ...)`

- [`opts_expression`](https://cynkra.github.io/constructive/reference/other-opts.md)`(constructor = c("default"), ...)`

- [`opts_externalptr`](https://cynkra.github.io/constructive/reference/opts_externalptr.md)`(constructor = c("default"), ...)`

- [`opts_FacetGrid`](https://cynkra.github.io/constructive/reference/other-opts.md)`(constructor = c("facet_grid", "ggproto", "next", "environment"), ...)`

- [`opts_FacetNull`](https://cynkra.github.io/constructive/reference/opts_FacetNull.md)`(constructor = c("facet_null", "next"), ...)`

- [`opts_FacetWrap`](https://cynkra.github.io/constructive/reference/other-opts.md)`(constructor = c("facet_wrap", "ggproto", "next", "environment"), ...)`

- [`opts_factor`](https://cynkra.github.io/constructive/reference/opts_factor.md)`(constructor = c("factor", "as_factor", "new_factor", "next", "integer"), ...)`

- [`opts_formula`](https://cynkra.github.io/constructive/reference/opts_formula.md)`(constructor = c("default", "formula", "as.formula", "new_formula", "next"), ..., environment = TRUE)`

- [`opts_function`](https://cynkra.github.io/constructive/reference/opts_function.md)`(constructor = c("function", "as.function", "new_function"), ..., environment = TRUE, srcref = FALSE, trim = NULL)`

- [`opts_ggplot`](https://cynkra.github.io/constructive/reference/opts_ggplot.md)`(constructor = c("ggplot", "next", "list"), ...)`

- [`opts_ggplot2_element_blank`](https://cynkra.github.io/constructive/reference/other-opts.md)`(constructor = c("element_blank", "next"), ...)`

- [`opts_ggplot2_element_geom`](https://cynkra.github.io/constructive/reference/other-opts.md)`(constructor = c("element_geom", "next"), ...)`

- [`opts_ggplot2_element_line`](https://cynkra.github.io/constructive/reference/other-opts.md)`(constructor = c("element_line", "next"), ...)`

- [`opts_ggplot2_element_point`](https://cynkra.github.io/constructive/reference/other-opts.md)`(constructor = c("element_point", "next"), ...)`

- [`opts_ggplot2_element_polygon`](https://cynkra.github.io/constructive/reference/other-opts.md)`(constructor = c("element_polygon", "next"), ...)`

- [`opts_ggplot2_element_rect`](https://cynkra.github.io/constructive/reference/other-opts.md)`(constructor = c("element_rect", "next"), ...)`

- [`opts_ggplot2_element_text`](https://cynkra.github.io/constructive/reference/other-opts.md)`(constructor = c("element_text", "next"), ...)`

- [`opts_ggplot2_ggplot`](https://cynkra.github.io/constructive/reference/opts_ggplot2_ggplot.md)`(constructor = c("ggplot", "next", "list"), ...)`

- [`opts_ggplot2_labels`](https://cynkra.github.io/constructive/reference/opts_ggplot2_labels.md)`(constructor = c("labs", "next"), ...)`

- [`opts_ggplot2_mapping`](https://cynkra.github.io/constructive/reference/other-opts.md)`(constructor = c("aes", "next", "list"), ...)`

- [`opts_ggplot2_margin`](https://cynkra.github.io/constructive/reference/other-opts.md)`(constructor = c("margin", "next", "double"), ...)`

- [`opts_ggplot2_theme`](https://cynkra.github.io/constructive/reference/other-opts.md)`(constructor = c("theme", "next", "list"), ...)`

- [`opts_ggproto`](https://cynkra.github.io/constructive/reference/other-opts.md)`(constructor = c("default", "ggproto", "next", "environment"), ...)`

- [`opts_grouped_df`](https://cynkra.github.io/constructive/reference/opts_grouped_df.md)`(constructor = c("default", "next", "list"), ...)`

- [`opts_Guide`](https://cynkra.github.io/constructive/reference/other-opts.md)`(constructor = c("default", "next", "environment"), ...)`

- [`opts_GuideAxis`](https://cynkra.github.io/constructive/reference/opts_GuideAxis.md)`(constructor = c("guide_axis", "next"), ...)`

- [`opts_GuideAxisLogticks`](https://cynkra.github.io/constructive/reference/opts_GuideAxisLogticks.md)`(constructor = c("guide_axis_logticks", "next"), ...)`

- [`opts_GuideAxisStack`](https://cynkra.github.io/constructive/reference/opts_GuideAxisStack.md)`(constructor = c("guide_axis_stack", "next"), ...)`

- [`opts_GuideAxisTheta`](https://cynkra.github.io/constructive/reference/opts_GuideAxisTheta.md)`(constructor = c("guide_axis_theta", "next"), ...)`

- [`opts_GuideBins`](https://cynkra.github.io/constructive/reference/opts_GuideBins.md)`(constructor = c("guide_bins", "next"), ...)`

- [`opts_GuideColourbar`](https://cynkra.github.io/constructive/reference/opts_GuideColourbar.md)`(constructor = c("guide_colorbar", "next"), ...)`

- [`opts_GuideColoursteps`](https://cynkra.github.io/constructive/reference/opts_GuideColoursteps.md)`(constructor = c("guide_colorsteps", "next"), ...)`

- [`opts_GuideCustom`](https://cynkra.github.io/constructive/reference/opts_GuideCustom.md)`(constructor = c("guide_custom", "next"), ...)`

- [`opts_GuideLegend`](https://cynkra.github.io/constructive/reference/opts_GuideLegend.md)`(constructor = c("guide_legend", "next"), ...)`

- [`opts_GuideNone`](https://cynkra.github.io/constructive/reference/opts_GuideNone.md)`(constructor = c("guide_none", "next"), ...)`

- [`opts_Guides`](https://cynkra.github.io/constructive/reference/opts_Guides.md)`(constructor = c("guides", "next"), ...)`

- [`opts_hexmode`](https://cynkra.github.io/constructive/reference/opts_hexmode.md)`(constructor = c("as.hexmode", "next"), ..., integer = FALSE)`

- [`opts_integer`](https://cynkra.github.io/constructive/reference/opts_integer.md)`(constructor = c("default"), ..., trim = NULL, fill = c("default", "rlang", "+", "...", "none"), compress = TRUE)`

- [`opts_integer64`](https://cynkra.github.io/constructive/reference/opts_integer64.md)`(constructor = c("as.integer64", "next", "double"), ...)`

- [`opts_labels`](https://cynkra.github.io/constructive/reference/other-opts.md)`(constructor = c("labs", "next", "list"), ...)`

- [`opts_language`](https://cynkra.github.io/constructive/reference/opts_language.md)`(constructor = c("default"), ...)`

- [`opts_Layer`](https://cynkra.github.io/constructive/reference/opts_Layer.md)`(constructor = c("default", "layer", "next", "environment"), ...)`

- [`opts_list`](https://cynkra.github.io/constructive/reference/opts_list.md)`(constructor = c("list", "list2"), ..., trim = NULL, fill = c("vector", "new_list", "+", "...", "none"))`

- [`opts_logical`](https://cynkra.github.io/constructive/reference/opts_logical.md)`(constructor = c("default"), ..., trim = NULL, fill = c("default", "rlang", "+", "...", "none"), compress = TRUE)`

- [`opts_margin`](https://cynkra.github.io/constructive/reference/other-opts.md)`(constructor = c("margin", "next", "double"), ...)`

- [`opts_matrix`](https://cynkra.github.io/constructive/reference/opts_matrix.md)`(constructor = c("matrix", "array", "cbind", "rbind", "next"), ...)`

- [`opts_mts`](https://cynkra.github.io/constructive/reference/opts_mts.md)`(constructor = c("ts", "next", "atomic"), ...)`

- [`opts_noquote`](https://cynkra.github.io/constructive/reference/other-opts.md)`(constructor = c("noquote", "next"), ...)`

- [`opts_NULL`](https://cynkra.github.io/constructive/reference/other-opts.md)`(constructor = "NULL", ...)`

- [`opts_numeric_version`](https://cynkra.github.io/constructive/reference/opts_numeric_version.md)`(constructor = c("numeric_version", "next", "list"), ...)`

- [`opts_object`](https://cynkra.github.io/constructive/reference/opts_object.md)`(constructor = c("prototype", "S7_object"), ...)`

- [`opts_octmode`](https://cynkra.github.io/constructive/reference/opts_octmode.md)`(constructor = c("as.octmode", "next"), ..., integer = FALSE)`

- [`opts_ordered`](https://cynkra.github.io/constructive/reference/opts_ordered.md)`(constructor = c("ordered", "factor", "new_ordered", "next", "integer"), ...)`

- [`opts_package_version`](https://cynkra.github.io/constructive/reference/opts_package_version.md)`(constructor = c("package_version", "next", "list"), ...)`

- [`opts_pairlist`](https://cynkra.github.io/constructive/reference/opts_pairlist.md)`(constructor = c("pairlist", "pairlist2"), ...)`

- [`opts_person`](https://cynkra.github.io/constructive/reference/other-opts.md)`(constructor = c("person", "next"), ...)`

- [`opts_POSIXct`](https://cynkra.github.io/constructive/reference/opts_POSIXct.md)`(constructor = c("as.POSIXct", ".POSIXct", "as_datetime", "as.POSIXct.numeric", "as_datetime.numeric", "next", "atomic"), ..., origin = "1970-01-01")`

- [`opts_POSIXlt`](https://cynkra.github.io/constructive/reference/opts_POSIXlt.md)`(constructor = c("as.POSIXlt", "next", "list"), ...)`

- [`opts_quosure`](https://cynkra.github.io/constructive/reference/opts_quosure.md)`(constructor = c("new_quosure", "next", "language"), ...)`

- [`opts_quosures`](https://cynkra.github.io/constructive/reference/opts_quosures.md)`(constructor = c("new_quosures", "next", "list"), ...)`

- [`opts_R_system_version`](https://cynkra.github.io/constructive/reference/opts_R_system_version.md)`(constructor = c("R_system_version", "next", "list"), ...)`

- [`opts_R6`](https://cynkra.github.io/constructive/reference/opts_R6.md)`(constructor = c("R6Class", "next"), ...)`

- [`opts_R6ClassGenerator`](https://cynkra.github.io/constructive/reference/opts_R6ClassGenerator.md)`(constructor = c("R6Class", "next"), ...)`

- [`opts_raw`](https://cynkra.github.io/constructive/reference/opts_raw.md)`(constructor = c("as.raw", "charToRaw"), ..., trim = NULL, fill = c("default", "rlang", "+", "...", "none"), compress = TRUE, representation = c("hexadecimal", "decimal"))`

- [`opts_rel`](https://cynkra.github.io/constructive/reference/other-opts.md)`(constructor = c("rel", "next", "double"), ...)`

- [`opts_rowwise_df`](https://cynkra.github.io/constructive/reference/opts_rowwise_df.md)`(constructor = c("default", "next", "list"), ...)`

- [`opts_S4`](https://cynkra.github.io/constructive/reference/opts_S4.md)`(constructor = c("new", "prototype"), ...)`

- [`opts_S7_any`](https://cynkra.github.io/constructive/reference/opts_S7_any.md)`(constructor = c("class_any", "next"), ...)`

- [`opts_S7_base_class`](https://cynkra.github.io/constructive/reference/opts_S7_base_class.md)`(constructor = c("S7", "next"), ...)`

- [`opts_S7_class`](https://cynkra.github.io/constructive/reference/opts_S7_class.md)`(constructor = c("new_class", "next"), ...)`

- [`opts_S7_external_generic`](https://cynkra.github.io/constructive/reference/opts_S7_external_generic.md)`(constructor = c("new_external_generic", "next"), ...)`

- [`opts_S7_generic`](https://cynkra.github.io/constructive/reference/opts_S7_generic.md)`(constructor = c("new_generic", "next"), ...)`

- [`opts_S7_object`](https://cynkra.github.io/constructive/reference/opts_S7_object.md)`(constructor = c("S7_object", "next"), ...)`

- [`opts_S7_property`](https://cynkra.github.io/constructive/reference/opts_S7_property.md)`(constructor = c("new_property", "next"), ...)`

- [`opts_S7_S3_class`](https://cynkra.github.io/constructive/reference/opts_S7_S3_class.md)`(constructor = c("new_S3_class", "next"), ...)`

- [`opts_S7_union`](https://cynkra.github.io/constructive/reference/opts_S7_union.md)`(constructor = c("default", "|", "new_union", "next"), ...)`

- [`opts_Scale`](https://cynkra.github.io/constructive/reference/other-opts.md)`(constructor = c("default", "next", "environment"), ...)`

- [`opts_ScalesList`](https://cynkra.github.io/constructive/reference/other-opts.md)`(constructor = c("ScalesList", "next", "list"), ...)`

- [`opts_simpleCondition`](https://cynkra.github.io/constructive/reference/other-opts.md)`(constructor = c("simpleCondition", "next"), ...)`

- [`opts_simpleError`](https://cynkra.github.io/constructive/reference/other-opts.md)`(constructor = c("simpleError", "next"), ...)`

- [`opts_simpleMessage`](https://cynkra.github.io/constructive/reference/other-opts.md)`(constructor = c("simpleMessage", "next"), ...)`

- [`opts_simpleUnit`](https://cynkra.github.io/constructive/reference/other-opts.md)`(constructor = c("unit", "next", "double"), ...)`

- [`opts_simpleWarning`](https://cynkra.github.io/constructive/reference/other-opts.md)`(constructor = c("simpleWarning", "next"), ...)`

- [`opts_tbl_df`](https://cynkra.github.io/constructive/reference/opts_tbl_df.md)`(constructor = c("tibble", "tribble", "next", "list"), ..., trailing_comma = TRUE, justify = c("left", "right", "centre", "none"), recycle = TRUE)`

- [`opts_theme`](https://cynkra.github.io/constructive/reference/other-opts.md)`(constructor = c("theme", "next", "list"), ...)`

- [`opts_ts`](https://cynkra.github.io/constructive/reference/opts_ts.md)`(constructor = c("ts", "next", "atomic"), ...)`

- [`opts_uneval`](https://cynkra.github.io/constructive/reference/other-opts.md)`(constructor = c("aes", "next", "list"), ...)`

- [`opts_vctrs_list_of`](https://cynkra.github.io/constructive/reference/opts_vctrs_list_of.md)`(constructor = c("list_of", "next", "list"), ...)`

- [`opts_waiver`](https://cynkra.github.io/constructive/reference/other-opts.md)`(constructor = c("waiver", "next", "list"), ...)`

- [`opts_warning`](https://cynkra.github.io/constructive/reference/other-opts.md)`(constructor = c("warningCondition", "next"), ...)`

- [`opts_weakref`](https://cynkra.github.io/constructive/reference/opts_weakref.md)`(constructor = c("new_weakref"), ...)`

- [`opts_xml_document`](https://cynkra.github.io/constructive/reference/opts_xml_document.md)`(constructor = c("default", "next"), ..., simplify = TRUE)`

- [`opts_xts`](https://cynkra.github.io/constructive/reference/opts_xts.md)`(constructor = c("as.xts.matrix", "next"), ...)`

- [`opts_yearmon`](https://cynkra.github.io/constructive/reference/opts_yearmon.md)`(constructor = c("as.yearmon", "yearmon", "next"), ...)`

- [`opts_yearqtr`](https://cynkra.github.io/constructive/reference/opts_yearqtr.md)`(constructor = c("as.yearqtr", "yearqtr", "next"), ...)`

- [`opts_zoo`](https://cynkra.github.io/constructive/reference/opts_zoo.md)`(constructor = c("zoo", "next"), ...)`

- [`opts_zooreg`](https://cynkra.github.io/constructive/reference/opts_zooreg.md)`(constructor = c("zooreg", "next"), ...)`

## Examples

``` r
construct(head(cars))
#> data.frame(speed = c(4, 4, 7, 7, 8, 9), dist = c(2, 10, 4, 22, 16, 10))
construct(head(cars), opts_data.frame("read.table"))
#> read.table(header = TRUE, text = "
#> speed dist
#>    4.   2.
#>    4.  10.
#>    7.   4.
#>    7.  22.
#>    8.  16.
#>    9.  10.
#> ")
construct(head(cars), opts_data.frame("next"))
#> list(speed = c(4, 4, 7, 7, 8, 9), dist = c(2, 10, 4, 22, 16, 10)) |>
#>   structure(row.names = c(NA, -6L), class = "data.frame")
construct(iris$Species)
#> factor(rep(c("setosa", "versicolor", "virginica"), each = 50L))
construct(iris$Species, opts_atomic(compress = FALSE), opts_factor("new_factor"))
#> vctrs::new_factor(
#>   c(
#>     1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
#>     1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
#>     1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L,
#>     2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L,
#>     2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L,
#>     2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L,
#>     3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L,
#>     3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L
#>   ),
#>   levels = c("setosa", "versicolor", "virginica")
#> )
construct_multi(list(a = head(cars), b = iris$Species))
#> a <- data.frame(speed = c(4, 4, 7, 7, 8, 9), dist = c(2, 10, 4, 22, 16, 10))
#> 
#> b <- factor(rep(c("setosa", "versicolor", "virginica"), each = 50L))
#> 
```
