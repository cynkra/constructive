# Other Opts Functions

These `opts_*()` functions are not extensively documented yet. Hopefully
the signature is self explanatory, if not please [raise an
issue](https://github.com/cynkra/constructive/issues)

## Usage

``` r
opts_NULL(constructor = "NULL", ...)

opts_bibentry(constructor = c("bibentry", "next"), ...)

opts_citationFooter(constructor = c("citFooter", "next"), ...)

opts_citationHeader(constructor = c("citHeader", "next"), ...)

opts_difftime(constructor = c("as.difftime", "next"), ...)

opts_error(constructor = c("errorCondition", "next"), ...)

opts_expression(constructor = c("default"), ...)

opts_CoordCartesian(
  constructor = c("default", "coord_cartesian", "next", "environment"),
  ...
)

opts_CoordFixed(constructor = c("coord_fixed", "next", "environment"), ...)

opts_CoordFlip(constructor = c("coord_flip", "next", "environment"), ...)

opts_CoordMap(constructor = c("coord_map", "next", "environment"), ...)

opts_CoordMunch(constructor = c("coord_munch", "next", "environment"), ...)

opts_CoordPolar(constructor = c("coord_polar", "next", "environment"), ...)

opts_CoordQuickmap(
  constructor = c("coord_quickmap", "next", "environment"),
  ...
)

opts_CoordSf(constructor = c("coord_sf", "next", "environment"), ...)

opts_CoordTrans(constructor = c("coord_trans", "next", "environment"), ...)

opts_FacetGrid(
  constructor = c("facet_grid", "ggproto", "next", "environment"),
  ...
)

opts_FacetWrap(
  constructor = c("facet_wrap", "ggproto", "next", "environment"),
  ...
)

opts_Guide(constructor = c("default", "next", "environment"), ...)

opts_Scale(constructor = c("default", "next", "environment"), ...)

opts_ScalesList(constructor = c("ScalesList", "next", "list"), ...)

opts_element_blank(constructor = c("element_blank", "next", "list"), ...)

opts_element_grob(constructor = c("element_grob", "next", "list"), ...)

opts_element_line(constructor = c("element_line", "next", "list"), ...)

opts_element_rect(constructor = c("element_rect", "next", "list"), ...)

opts_element_render(constructor = c("element_render", "next", "list"), ...)

opts_element_text(constructor = c("element_text", "next", "list"), ...)

opts_ggplot2_element_blank(constructor = c("element_blank", "next"), ...)

opts_ggplot2_element_geom(constructor = c("element_geom", "next"), ...)

opts_ggplot2_element_line(constructor = c("element_line", "next"), ...)

opts_ggplot2_element_point(constructor = c("element_point", "next"), ...)

opts_ggplot2_element_polygon(constructor = c("element_polygon", "next"), ...)

opts_ggplot2_element_rect(constructor = c("element_rect", "next"), ...)

opts_ggplot2_element_text(constructor = c("element_text", "next"), ...)

opts_ggplot2_mapping(constructor = c("aes", "next", "list"), ...)

opts_ggplot2_margin(constructor = c("margin", "next", "double"), ...)

opts_ggplot2_theme(constructor = c("theme", "next", "list"), ...)

opts_ggproto(constructor = c("default", "ggproto", "next", "environment"), ...)

opts_labels(constructor = c("labs", "next", "list"), ...)

opts_margin(constructor = c("margin", "next", "double"), ...)

opts_rel(constructor = c("rel", "next", "double"), ...)

opts_theme(constructor = c("theme", "next", "list"), ...)

opts_uneval(constructor = c("aes", "next", "list"), ...)

opts_waiver(constructor = c("waiver", "next", "list"), ...)

opts_noquote(constructor = c("noquote", "next"), ...)

opts_person(constructor = c("person", "next"), ...)

opts_simpleCondition(constructor = c("simpleCondition", "next"), ...)

opts_simpleError(constructor = c("simpleError", "next"), ...)

opts_simpleMessage(constructor = c("simpleMessage", "next"), ...)

opts_simpleUnit(constructor = c("unit", "next", "double"), ...)

opts_simpleWarning(constructor = c("simpleWarning", "next"), ...)

opts_warning(constructor = c("warningCondition", "next"), ...)
```

## Arguments

- constructor:

  String. Method used to construct the object, often the name of a
  function.

- ...:

  Additional options used by user defined constructors through the
  `opts` object
