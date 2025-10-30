test_that("contains_self_reference", {
  expect_false(contains_self_reference(1))
  expect_false(contains_self_reference(baseenv()))

  env <- new.env(parent = baseenv())
  # redundant definition is ok, circular is not!
  expect_false(contains_self_reference(list(env, env)))

  env <- new.env(parent = baseenv())
  env$e <- env
  expect_true(contains_self_reference(env))
  expect_true(contains_self_reference(list(env)))

  env <- new.env(parent = baseenv())
  env$x <- list(env)
  expect_true(contains_self_reference(env))

  env <- new.env(parent = baseenv())
  env$x <- structure(1, foo = env)
  expect_true(contains_self_reference(env))

  env <- new.env(parent = baseenv())
  attr(env, "foo") <- env
  expect_true(contains_self_reference(env))

  env <- new.env(parent = baseenv())
  env$f <- function() NULL
  environment(env$f) <- env
  expect_true(contains_self_reference(env))
  expect_false(contains_self_reference(env, check_function = FALSE))

  parent <- new.env()
  env <- new.env(parent = parent)
  attr(parent, "foo") <- env
  expect_true(contains_self_reference(env))
})

test_that("self reference fails properly", {
  env <- new.env(parent = baseenv())
  env$x <- list(env)
  expect_error(construct(env, opts_environment("list2env")), "self-references")
  expect_error(construct(env, opts_environment("new_environment")), "self-references")
  expect_error(construct(env, opts_environment("as.environment")), "self-references")
  expect_snapshot(construct(env, opts_environment("predefine")))

  env <- new.env(parent = baseenv())
  env$x <- structure(1, foo = env)
  expect_error(construct(env, opts_environment("list2env")), "self-references")
  expect_error(construct(env, opts_environment("new_environment")), "self-references")
  expect_error(construct(env, opts_environment("as.environment")), "self-references")
  expect_snapshot(construct(env, opts_environment("predefine")))

  env <- new.env(parent = baseenv())
  attr(env, "foo") <- env
  expect_error(construct(env, opts_environment("list2env")), "self-references")
  expect_error(construct(env, opts_environment("new_environment")), "self-references")
  expect_error(construct(env, opts_environment("as.environment")), "self-references")
  expect_snapshot(construct(env, opts_environment("predefine")))

  env <- new.env(parent = baseenv())
  env$f <- function() NULL
  environment(env$f) <- env
  expect_error(construct(env, opts_environment("list2env")), "self-references")
  expect_error(construct(env, opts_environment("new_environment")), "self-references")
  expect_error(construct(env, opts_environment("as.environment")), "self-references")
  expect_no_error(construct(
      env,
      opts_environment("list2env"),
      opts_function(environment = FALSE),
      check = FALSE))
  expect_no_error(construct(
    env,
    opts_environment("new_environment"),
    opts_function(environment = FALSE),
    check = FALSE))
  expect_no_error(construct(
    env,
    opts_environment("as.environment"),
    opts_function(environment = FALSE),
    check = FALSE))
  expect_snapshot(construct(env, opts_environment("predefine")))

  parent <- new.env(parent = baseenv())
  env <- new.env(parent = parent)
  attr(parent, "foo") <- env
  expect_no_error(construct(env, opts_environment("list2env"), check = FALSE))
  expect_no_error(construct(env, opts_environment("new_environment"), check = FALSE))
  expect_no_error(construct(env, opts_environment("as.environment"), check = FALSE))
  expect_error(construct(env, opts_environment("list2env", recurse = TRUE)), "self-references")
  expect_error(construct(env, opts_environment("new_environment", recurse = TRUE)), "self-references")
  # FIXME: this doesn't work yet but it's quite contrived!
  # expect_snapshot(construct(env, opts_environment(predefine = TRUE)))
})
