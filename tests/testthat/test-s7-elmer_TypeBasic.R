test_that("ellmer::TypeBasic", {
  expect_construct(
    ellmer::type_array(
      items = ellmer::type_object(
        x = ellmer::type_boolean(),
        y = ellmer::type_string(),
        z = ellmer::type_number(),
        json = ellmer::type_from_schema("[1,2]")
      )
    )
  )
  expect_construct(
    ellmer::type_array(
      "An array",
      items = ellmer::type_object(
        "An object",
        x = ellmer::type_boolean("A boolean", required = FALSE),
        y = ellmer::type_string("A string"),
        z = ellmer::type_number("A number"),
        json = ellmer::type_from_schema("[1,2]"),
        .additional_properties = TRUE
      ),
      required = FALSE
    )
  )

  expect_construct(
    ellmer::type_array(
      items = ellmer::type_object(
        x = ellmer::type_boolean(),
        y = ellmer::type_string(),
        z = ellmer::type_number(),
        json = ellmer::type_from_schema("[1,2]")
      )
    ),
    ellmer::TypeArray(
      items = ellmer::TypeObject(
        properties = list(
          x = ellmer::TypeBasic(type = "boolean"),
          y = ellmer::TypeBasic(type = "string"),
          z = ellmer::TypeBasic(type = "number"),
          json = ellmer::TypeJsonSchema(json = list(1L, 2L))
        ),
        additional_properties = FALSE
      )
    ),
    opts_ellmer_TypeBasic("TypeBasic"),
    opts_ellmer_TypeArray("TypeArray"),
    opts_ellmer_TypeObject("TypeObject"),
    opts_ellmer_TypeJsonSchema("TypeJsonSchema")
  )

  expect_construct(
    ellmer::type_array(
      "An array",
      items = ellmer::type_object(
        "An object",
        x = ellmer::type_boolean("A boolean", required = FALSE),
        y = ellmer::type_string("A string"),
        z = ellmer::type_number("A number"),
        json = ellmer::type_from_schema("[1,2]"),
        .additional_properties = TRUE
      ),
      required = FALSE
    ),
    ellmer::TypeArray(
      "An array",
      items = ellmer::TypeObject(
        "An object",
        properties = list(
          x = ellmer::TypeBasic("A boolean", required = FALSE, type = "boolean"),
          y = ellmer::TypeBasic("A string", type = "string"),
          z = ellmer::TypeBasic("A number", type = "number"),
          json = ellmer::TypeJsonSchema(json = list(1L, 2L))
        )
      ),
      required = FALSE
    ),
    opts_ellmer_TypeBasic("TypeBasic"),
    opts_ellmer_TypeArray("TypeArray"),
    opts_ellmer_TypeObject("TypeObject"),
    opts_ellmer_TypeJsonSchema("TypeJsonSchema")
  )
})
