test_that("deparse_call()", {
  expect_snapshot({
    deparse_call(call("::", 1, 2), style = FALSE)
    deparse_call(call("::", "a", quote(b)), style = FALSE)
    deparse_call(call("::", quote(a), "b"), style = FALSE)
    deparse_call(call(":::", 1, 2), style = FALSE)
    deparse_call(call(":::", "a", quote(b)), style = FALSE)
    deparse_call(call(":::", quote(a), "b"), style = FALSE)
    deparse_call(call("+", 1, 2, 3), style = FALSE)
    deparse_call(call("+", 1, 2), style = FALSE)
    deparse_call(call("+", 1), style = FALSE)
    deparse_call(call("+"), style = FALSE)
    deparse_call(call("$", "a", "b", "c"), style = FALSE)
    deparse_call(call("$", "a", "b"), style = FALSE)
    deparse_call(call("$", quote(a), "b"), style = FALSE)
    deparse_call(call("$", quote(a), quote(b)), style = FALSE)
    deparse_call(call("$", "a", 1), style = FALSE)
    deparse_call(call("$", 1, "b"), style = FALSE)
    deparse_call(call("$"), style = FALSE)
    deparse_call(call("$"), style = FALSE)
    deparse_call(call(":", 1, 2, 3), style = FALSE)
    deparse_call(call(":", 1, 2), style = FALSE)
    deparse_call(call(":", 1), style = FALSE)
    deparse_call(call(":"), style = FALSE)
    deparse_call(call("(", 1, 2), style = FALSE)
    deparse_call(call("(", 1), style = FALSE)
    deparse_call(call("("), style = FALSE)
    deparse_call(call("non-syntactic", 1), style = FALSE)

    deparse_call(quote(foo(bar(baz(x), 1), arg = 2, empty=)), style = FALSE)
    deparse_call(quote(foo(bar(baz(x), 1), arg = 2, empty=)), pipe = TRUE, style = FALSE)
    # don't pipe if named arg since we can't be sure it's the same from static analysis
    deparse_call(quote(foo(a=1, 2)), pipe = TRUE, style = FALSE)

    deparse_call(quote(function(x,y=1,z=a) {x+y}), style = FALSE)
    deparse_call(quote(function(x,y=1,z=a) {x+y}), one_liner = TRUE, style = FALSE)

    deparse_call(quote(if (cond) this else that), style = FALSE)
    deparse_call(quote(if (cond) {this} else {that}), style = FALSE)
    deparse_call(quote(while (cond) this), style = FALSE)
    deparse_call(quote(while (cond) {this}), style = FALSE)
    deparse_call(quote(for (i in this) that), style = FALSE)
    deparse_call(quote(for (i in this) {that}), style = FALSE)
    deparse_call(quote(repeat this), style = FALSE)
    deparse_call(quote(repeat {this}), style = FALSE)

    # non syntatic symbols
    deparse_call(quote(`*a*`))

    # brackets and function calls with names
    deparse_call(quote(a(b=1, c)))
    deparse_call(quote(a[b=1, c]))
    deparse_call(quote(a[[b=1, c]]))
    deparse_call(quote(a(bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb=1, c)))
    deparse_call(quote(a[bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb=1, c]))
    # looks odd, but that's on {styler} : https://github.com/r-lib/styler/issues/1029
    deparse_call(quote(a[[bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb=1, c]]))

    # Multiline calls
    deparse_call(quote(a(b(c12345678901234567890123456789012345678901234567890123456789012345678901234567890))))
    deparse_call(quote(a({c12345678901234567890123456789012345678901234567890123456789012345678901234567890}, b)))

    # function with non syntactic formal names
    deparse_call(quote(function(`_x`) `_x`))

    # call to function with too many args and a first arg that is not a pairlist
    # this cannot be tested because of testthat/rlang limitations
    # deparse_call(quote(`function`(a(b, c), d, e)))

    # non-syntactig argument name
    deparse_call(quote(list(`a + b` = a + b)))

    # non syntactic function calls
    deparse_call(quote((`boo<-`)(x)))
    deparse_call(quote(`boo<-`[[1]](x)))
  })

  expect_snapshot({
    # Avoid testthat corruption
    deparse_call(eval(str2lang("quote(`=`(x, 1))")))
    deparse_call(eval(str2lang("quote(list(`=`(x, 1)))")))
    deparse_call(eval(str2lang("quote((`=`(x, 1)))")))
    deparse_call(quote(list(x = 1)))
    deparse_call(quote({x = 1}))
  })

  expect_snapshot({
    deparse_call(quote({{x}}), style = FALSE) # proper tunnel
    deparse_call(quote({{1}}), style = FALSE) # not a symbol
    deparse_call(quote({{1}}), one_liner = TRUE, style = FALSE)
    deparse_call('"')
    deparse_call('"', escape = TRUE)
    deparse_call("ü")
    deparse_call("ü", unicode_representation = "latin")
  })

  expect_snapshot({
    deparse_call(quote(1 -> x <- 2))
    deparse_call(quote(1 -> if(TRUE) 1))
    deparse_call(quote(1 -> for(i in j) 1))
    deparse_call(quote(1 -> while(TRUE) 1))
    deparse_call(quote(1 -> repeat 1))
  })
})

test_that("deparse_call() for R >= 4.1", {
  # Due to bypass.R
  skip_if(base::`<`(getRversion(), "4.1"))
  expect_snapshot({
    deparse_call(quote(`🐶`), style = FALSE)
    deparse_call(quote(`🐶`), unicode_representation = "unicode")
  })
})

test_that("deparse_call() fails when the caller is empty", {
  # Note this prints a "fake" rstudio error when called manually
  call <- substitute(X(), list(X = quote(expr = )))
  expect_error(deparse_call(call), regexp = "Found empty symbol")
  call <- substitute({X(1, 2)}, list(X = quote(expr = )))
  expect_error(deparse_call(call), regexp = "Found empty symbol")
})

test_that("deparse_call() fails when the sole arg is empty", {
  expect_error(deparse_call(call("fun", quote(expr = ))), regexp = "Found empty symbol")
  expect_error(deparse_call(call("+", quote(expr = ))), regexp = "Found empty symbol")
})

test_that("square brackets", {
  expect_snapshot({
    deparse_call(call("[", 1, 2, 3), style = FALSE)
    deparse_call(call("[", 1, 2), style = FALSE)
    deparse_call(call("[", 1), style = FALSE)
    deparse_call(call("["), style = FALSE)
    deparse_call(call("[[", 1, 2, 3), style = FALSE)
    deparse_call(call("[[", 1, 2), style = FALSE)
    deparse_call(call("[[", 1), style = FALSE)
    deparse_call(call("[["), style = FALSE)
    deparse_call(call("[", quote(expr=), quote(expr=)), style = FALSE)
    deparse_call(call("[", 1, quote(expr=)), style = FALSE)
    deparse_call(call("[", quote(a+b), 1), style = FALSE)
    deparse_call(quote(a$b[[c]]))
    deparse_call(quote(a[[b]]$c))
    deparse_call(quote(a[[b$c]]))
    deparse_call(call("[", quote(while (TRUE) {}), 1), style = FALSE)
    deparse_call(call("[", quote(if (TRUE) {}), 1), style = FALSE)
    deparse_call(call("[", quote(for (a in b) {}), 1), style = FALSE)
    deparse_call(call("[", quote(repeat {}), 1), style = FALSE)
    deparse_call(call("[", quote(function() {}), 1), style = FALSE)
    deparse_call(call("[", call("function", 1,2), 1), style = FALSE)
  })
})

test_that("curly braces", {
  expect_snapshot({
    deparse_call(call("{"), style = FALSE)
    deparse_call(call("{", 1, 2), style = FALSE)
    deparse_call(call("{", 1, 2), one_liner = TRUE, style = FALSE)
    deparse_call(call("{", 1, quote(expr = )), style = FALSE)
  })
})

test_that("Use lisp notation when the caller expr calls a control flow construct", {
  expect_snapshot({
    deparse_call(substitute(X(Y), list(X = quote(if (TRUE) {}), Y = 1)))
    deparse_call(substitute(X(Y), list(X = quote(while (TRUE) {}), Y = 1)))
    deparse_call(substitute(X(Y), list(X = quote(for (a in b) {}), Y = 1)))
    deparse_call(substitute(X(Y), list(X = quote(repeat {}), Y = 1)))
  })
})

test_that("Operator precedence is well handled", {
  expect_snapshot({
    deparse_call(str2lang("`^`(`+`(a, b), c)"))
    deparse_call(str2lang("`+`(`^`(a, b), c)"))
    deparse_call(str2lang("`%in%`(`*`(a, b), c)"))
    deparse_call(str2lang("`*`(`%in%`(a, b), c)"))
    deparse_call(str2lang("`+`(`+`(1, 2), 4)"))
    deparse_call(str2lang("`-`(1+2)"))
    deparse_call(str2lang("`<-`(`<<-`(1, 2), 4)"))
    deparse_call(str2lang("`+`(x, y)(z)"))
    deparse_call(quote(x <- a::b(y)))
    deparse_call(quote(x <- a:::b(y)))
    deparse_call(quote(x <- a$b(y)))
    deparse_call(quote(x <- a@b(y)))
    deparse_call(quote(x <- a::b$c(y)))
    deparse_call(str2lang("`^`(`^`(1, 2), 4)"))
    deparse_call(str2lang("`^`(4, `^`(1, 2))"))
    deparse_call(str2lang("`+`(4, `+`(1, 2))"))
    deparse_call(substitute(X + Y, list(X = quote(repeat {}), Y = 1)))
    deparse_call(substitute(X + Y, list(X = 1, Y = quote(repeat {}))))
    deparse_call(substitute(X ? Y, list(X = quote(repeat {}), Y = 1)))
    deparse_call(substitute(X ? Y, list(X = 1, Y = quote(repeat {}))))
    deparse_call(substitute(X$Y, list(X = quote(repeat {}), Y = 1)))
    deparse_call(substitute(X$Y, list(X = 1, Y = quote(repeat {}))))
    deparse_call(substitute(X(Y), list(X = quote(repeat {}), Y = 1)))
    deparse_call(substitute(X[Y], list(X = quote(repeat {}), Y = 1)))
    deparse_call(quote(+repeat {}))
    deparse_call(quote(+repeat {}))
  })
})
