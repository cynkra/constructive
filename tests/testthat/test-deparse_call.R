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
    deparse_call(call("[", 1, 2, 3), style = FALSE)
    deparse_call(call("[", 1, 2), style = FALSE)
    deparse_call(call("[", 1), style = FALSE)
    deparse_call(call("["), style = FALSE)
    deparse_call(call("[[", 1, 2, 3), style = FALSE)
    deparse_call(call("[[", 1, 2), style = FALSE)
    deparse_call(call("[[", 1), style = FALSE)
    deparse_call(call("[["), style = FALSE)
    deparse_call(call("(", 1, 2), style = FALSE)
    deparse_call(call("(", 1), style = FALSE)
    deparse_call(call("("), style = FALSE)
    deparse_call(call("{"), style = FALSE)
    deparse_call(call("{", 1, 2), style = FALSE)
    deparse_call(call("{", 1, 2), one_liner = TRUE, style = FALSE)
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

    # non-syntactig argument name
    deparse_call(quote(list(`a + b` = a + b)))

    # non syntactic function calls
    deparse_call(quote((`boo<-`)(x)))
    deparse_call(quote(`boo<-`[[1]](x)))
  })

  expect_snapshot({
    # the following line cannot be tested with testthat because of their own
    # faulty deparsing
    # deparse_call(quote(`=`(x, 1)))
    deparse_call(quote(list(`=`(x, 1))))
    deparse_call(quote((`=`(x, 1))))
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

  expect_snapshot({
    construct(quote(`^`(`+`(a, b), c)))
    construct(quote(`+`(`^`(a, b), c)))
    construct(quote(`%in%`(`*`(a, b), c)))
    construct(quote(`*`(`%in%`(a, b), c)))
    construct(quote(`+`(`+`(1, 2), 4)))
    construct(quote(`-`(1+2)))
    construct(quote(`<-`(`<<-`(1, 2), 4)))
    construct(quote(`+`(x, y)(z)))
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

test_that("high precedence infix ops in deparse_call()", {
  expect_snapshot({
    deparse_call(quote(x <- a::b(y)))
    deparse_call(quote(x <- a:::b(y)))
    deparse_call(quote(x <- a$b(y)))
    deparse_call(quote(x <- a@b(y)))
    deparse_call(quote(x <- a::b$c(y)))
  })
})

