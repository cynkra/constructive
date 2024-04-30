test_that("person", {
  expect_snapshot({
    construct(person(given = "given_", role = "aut"))
  })
})
