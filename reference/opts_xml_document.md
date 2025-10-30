# Constructive options for class 'xml_document'

These options will be used on objects of class 'xml_document'.

## Usage

``` r
opts_xml_document(constructor = c("default", "next"), ..., simplify = TRUE)
```

## Arguments

- constructor:

  String. Name of the constructor, often the function used to construct
  the object.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

- simplify:

  Whether to remove the "meta http-equiv" and "!DOCTYPE" tags from the
  input if they're the default ones.

## Value

An object of class
\<constructive_options/constructive_options_xml_document\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"default"` (default): We build the object using
  [`xml2::read_xml()`](http://xml2.r-lib.org/reference/read_xml.md) or
  [`xml2::read_html()`](http://xml2.r-lib.org/reference/read_xml.md) on
  a string.

- `"next"` : Use the constructor for the next supported class.
