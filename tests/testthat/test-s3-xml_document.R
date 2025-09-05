test_that("xml_document", {
  expect_snapshot({
    x <- xml2::read_xml("<foo><bar /></foo>")
    construct(x)
    construct(x, opts_xml_document(simplify = FALSE))
    construct(x, opts_xml_document("next"), check = FALSE)
    y <- xml2::read_html("<foo><bar /></foo>")
    construct(y)
    construct(y, opts_xml_document(simplify = FALSE))
    construct(y, opts_xml_document("next"), check = FALSE)
  })
})
