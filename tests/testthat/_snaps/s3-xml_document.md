# xml_document

    Code
      x <- xml2::read_xml("<foo><bar /></foo>")
      construct(x)
    Output
      xml2::read_xml("\n<foo>\n  <bar/>\n</foo>\n")
    Code
      construct(x, opts_xml_document(simplify = FALSE))
    Output
      xml2::read_xml('<?xml version="1.0" encoding="UTF-8"?>\n<foo>\n  <bar/>\n</foo>\n')
    Code
      construct(x, opts_xml_document("next"), check = FALSE)
    Output
      list(
        node = constructive::.xptr("0x123456789"),
        doc = constructive::.xptr("0x123456789")
      ) |>
        structure(class = c("xml_document", "xml_node"))
    Code
      y <- xml2::read_html("<foo><bar /></foo>")
      construct(y)
    Output
      xml2::read_html("<html><body><foo><bar></bar></foo></body></html>")
    Code
      construct(y, opts_xml_document(simplify = FALSE))
    Output
      xml2::read_html(
        '<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN" "http://www.w3.org/TR/REC-html40/loose.dtd">\n<html><body><foo><bar></bar></foo></body></html>\n'
      )
    Code
      construct(y, opts_xml_document("next"), check = FALSE)
    Output
      list(
        node = constructive::.xptr("0x123456789"),
        doc = constructive::.xptr("0x123456789")
      ) |>
        structure(class = c("xml_document", "xml_node"))

