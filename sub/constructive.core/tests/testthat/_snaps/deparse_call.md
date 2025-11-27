# deparse_call()

    Code
      deparse_call(call("::", 1, 2), style = FALSE)
    Output
      [1] "`::`(1, 2)"
    Code
      deparse_call(call("::", "a", quote(b)), style = FALSE)
    Output
      [1] "\"a\"::b"
    Code
      deparse_call(call("::", quote(a), "b"), style = FALSE)
    Output
      [1] "a::\"b\""
    Code
      deparse_call(call(":::", 1, 2), style = FALSE)
    Output
      [1] "`:::`(1, 2)"
    Code
      deparse_call(call(":::", "a", quote(b)), style = FALSE)
    Output
      [1] "\"a\":::b"
    Code
      deparse_call(call(":::", quote(a), "b"), style = FALSE)
    Output
      [1] "a:::\"b\""
    Code
      deparse_call(call("+", 1, 2, 3), style = FALSE)
    Output
      [1] "`+`(1, 2, 3)"
    Code
      deparse_call(call("+", 1, 2), style = FALSE)
    Output
      [1] "1 + 2"
    Code
      deparse_call(call("+", 1), style = FALSE)
    Output
      [1] "+1"
    Code
      deparse_call(call("+"), style = FALSE)
    Output
      [1] "`+`()"
    Code
      deparse_call(call("$", "a", "b", "c"), style = FALSE)
    Output
      [1] "`$`(\"a\", \"b\", \"c\")"
    Code
      deparse_call(call("$", "a", "b"), style = FALSE)
    Output
      [1] "\"a\"$\"b\""
    Code
      deparse_call(call("$", quote(a), "b"), style = FALSE)
    Output
      [1] "a$\"b\""
    Code
      deparse_call(call("$", quote(a), quote(b)), style = FALSE)
    Output
      [1] "a$b"
    Code
      deparse_call(call("$", "a", 1), style = FALSE)
    Output
      [1] "`$`(\"a\", 1)"
    Code
      deparse_call(call("$", 1, "b"), style = FALSE)
    Output
      [1] "1$\"b\""
    Code
      deparse_call(call("$"), style = FALSE)
    Output
      [1] "`$`()"
    Code
      deparse_call(call("$"), style = FALSE)
    Output
      [1] "`$`()"
    Code
      deparse_call(call(":", 1, 2, 3), style = FALSE)
    Output
      [1] "`:`(1, 2, 3)"
    Code
      deparse_call(call(":", 1, 2), style = FALSE)
    Output
      [1] "1:2"
    Code
      deparse_call(call(":", 1), style = FALSE)
    Output
      [1] "`:`(1)"
    Code
      deparse_call(call(":"), style = FALSE)
    Output
      [1] "`:`()"
    Code
      deparse_call(call("(", 1, 2), style = FALSE)
    Output
      [1] "`(`(1, 2)"
    Code
      deparse_call(call("(", 1), style = FALSE)
    Output
      [1] "(1)"
    Code
      deparse_call(call("("), style = FALSE)
    Output
      [1] "`(`()"
    Code
      deparse_call(call("non-syntactic", 1), style = FALSE)
    Output
      [1] "`non-syntactic`(1)"
    Code
      deparse_call(quote(foo(bar(baz(x), 1), arg = 2, empty = )), style = FALSE)
    Output
      [1] "foo(bar(baz(x), 1), arg = 2, empty = )"
    Code
      deparse_call(quote(foo(bar(baz(x), 1), arg = 2, empty = )), pipe = TRUE, style = FALSE)
    Output
      [1] "x |> baz() |> bar(1) |> foo(arg = 2, empty = )"
    Code
      deparse_call(quote(foo(a = 1, 2)), pipe = TRUE, style = FALSE)
    Output
      [1] "foo(a = 1, 2)"
    Code
      deparse_call(quote(function(x, y = 1, z = a) {
        x + y
      }), style = FALSE)
    Output
      [1] "function(x, y = 1, z = a) {\n  x + y\n}"
    Code
      deparse_call(quote(function(x, y = 1, z = a) {
        x + y
      }), one_liner = TRUE, style = FALSE)
    Output
      [1] "function(x, y = 1, z = a) {x + y}"
    Code
      deparse_call(quote(if (cond) this else that), style = FALSE)
    Output
      [1] "if (cond) this else that"
    Code
      deparse_call(quote(if (cond) {
        this
      } else {
        that
      }), style = FALSE)
    Output
      [1] "if (cond) {\n  this\n} else {\n  that\n}"
    Code
      deparse_call(quote(while (cond) this), style = FALSE)
    Output
      [1] "while (cond) this"
    Code
      deparse_call(quote(while (cond) {
        this
      }), style = FALSE)
    Output
      [1] "while (cond) {\n  this\n}"
    Code
      deparse_call(quote(for (i in this) that), style = FALSE)
    Output
      [1] "for (i in this) that"
    Code
      deparse_call(quote(for (i in this) {
        that
      }), style = FALSE)
    Output
      [1] "for (i in this) {\n  that\n}"
    Code
      deparse_call(quote(repeat this), style = FALSE)
    Output
      [1] "repeat this"
    Code
      deparse_call(quote(repeat {
        this
      }), style = FALSE)
    Output
      [1] "repeat {\n  this\n}"
    Code
      deparse_call(quote(`*a*`))
    Output
      `*a*`
    Code
      deparse_call(quote(a(b = 1, c)))
    Output
      a(b = 1, c)
    Code
      deparse_call(quote(a[b = 1, c]))
    Output
      a[b = 1, c]
    Code
      deparse_call(quote(a[[b = 1, c]]))
    Output
      a[[b = 1, c]]
    Code
      deparse_call(quote(a(
        bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb = 1,
        c)))
    Output
      a(
        bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb = 1,
        c
      )
    Code
      deparse_call(quote(a[
        bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb = 1,
        c]))
    Output
      a[
        bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb = 1,
        c
      ]
    Code
      deparse_call(quote(a[[
        bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb = 1,
        c]]))
    Output
      a[[
        bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb = 1,
        c
      ]]
    Code
      deparse_call(quote(a(b(
        c12345678901234567890123456789012345678901234567890123456789012345678901234567890))))
    Output
      a(
        b(
          c12345678901234567890123456789012345678901234567890123456789012345678901234567890
        )
      )
    Code
      deparse_call(quote(a({
        c12345678901234567890123456789012345678901234567890123456789012345678901234567890
      }, b)))
    Output
      a(
        {
          c12345678901234567890123456789012345678901234567890123456789012345678901234567890
        },
        b
      )
    Code
      deparse_call(quote(function(`_x`) `_x`))
    Output
      function(`_x`) `_x`
    Code
      deparse_call(quote(list(`a + b` = a + b)))
    Output
      list(`a + b` = a + b)
    Code
      deparse_call(quote((`boo<-`)(x)))
    Output
      (`boo<-`)(x)
    Code
      deparse_call(quote(`boo<-`[[1]](x)))
    Output
      `boo<-`[[1]](x)

---

    Code
      deparse_call(eval(str2lang("quote(`=`(x, 1))")))
    Output
      x = 1
    Code
      deparse_call(eval(str2lang("quote(list(`=`(x, 1)))")))
    Output
      list(`=`(x, 1))
    Code
      deparse_call(eval(str2lang("quote((`=`(x, 1)))")))
    Output
      (x = 1)
    Code
      deparse_call(quote(list(x = 1)))
    Output
      list(x = 1)
    Code
      deparse_call(quote({
        x = 1
      }))
    Output
      {
        x = 1
      }

---

    Code
      deparse_call(quote({{ x }}), style = FALSE)
    Output
      [1] "{{ x }}"
    Code
      deparse_call(quote({
        {
          1
        }
      }), style = FALSE)
    Output
      [1] "{\n  {\n    1\n  }\n}"
    Code
      deparse_call(quote({
        {
          1
        }
      }), one_liner = TRUE, style = FALSE)
    Output
      [1] "{{1}}"
    Code
      deparse_call("\"")
    Output
      '"'
    Code
      deparse_call("\"", escape = TRUE)
    Output
      "\""
    Code
      deparse_call("ü")
    Output
      "\U{FC}"
    Code
      deparse_call("ü", unicode_representation = "latin")
    Output
      "ü"

---

    Code
      deparse_call(quote((x <- 1) <- 2))
    Output
      (x <- 1) <- 2
    Code
      deparse_call(quote(if (TRUE) 1 <- 1))
    Output
      if (TRUE) 1 <- 1
    Code
      deparse_call(quote(for (i in j) 1 <- 1))
    Output
      for (i in j) 1 <- 1
    Code
      deparse_call(quote(while (TRUE) 1 <- 1))
    Output
      while (TRUE) 1 <- 1
    Code
      deparse_call(quote(repeat 1 <- 1))
    Output
      repeat 1 <- 1

# deparse_call() for R >= 4.1

    Code
      deparse_call(quote(`🐶`), style = FALSE)
    Output
      [1] "`\\xf0\\x9f\\x90\\xb6`"
    Code
      deparse_call(quote(`🐶`), unicode_representation = "unicode")
    Output
      `🐶`

# square brackets

    Code
      deparse_call(call("[", 1, 2, 3), style = FALSE)
    Output
      [1] "1[2, 3]"
    Code
      deparse_call(call("[", 1, 2), style = FALSE)
    Output
      [1] "1[2]"
    Code
      deparse_call(call("[", 1), style = FALSE)
    Output
      [1] "`[`(1)"
    Code
      deparse_call(call("["), style = FALSE)
    Output
      [1] "`[`()"
    Code
      deparse_call(call("[[", 1, 2, 3), style = FALSE)
    Output
      [1] "1[[2, 3]]"
    Code
      deparse_call(call("[[", 1, 2), style = FALSE)
    Output
      [1] "1[[2]]"
    Code
      deparse_call(call("[[", 1), style = FALSE)
    Output
      [1] "`[[`(1)"
    Code
      deparse_call(call("[["), style = FALSE)
    Output
      [1] "`[[`()"
    Code
      deparse_call(call("[", quote(expr = ), quote(expr = )), style = FALSE)
    Output
      [1] "`[`(, )"
    Code
      deparse_call(call("[", 1, quote(expr = )), style = FALSE)
    Output
      [1] "1[]"
    Code
      deparse_call(call("[", quote(a + b), 1), style = FALSE)
    Output
      [1] "`[`(a + b, 1)"
    Code
      deparse_call(quote(a$b[[c]]))
    Output
      a$b[[c]]
    Code
      deparse_call(quote(a[[b]]$c))
    Output
      a[[b]]$c
    Code
      deparse_call(quote(a[[b$c]]))
    Output
      a[[b$c]]
    Code
      deparse_call(call("[", quote(while (TRUE) { }), 1), style = FALSE)
    Output
      [1] "`[`(while (TRUE) { }, 1)"
    Code
      deparse_call(call("[", quote(if (TRUE) { }), 1), style = FALSE)
    Output
      [1] "`[`(if (TRUE) { }, 1)"
    Code
      deparse_call(call("[", quote(for (a in b) { }), 1), style = FALSE)
    Output
      [1] "`[`(for (a in b) { }, 1)"
    Code
      deparse_call(call("[", quote(repeat { }), 1), style = FALSE)
    Output
      [1] "`[`(repeat { }, 1)"
    Code
      deparse_call(call("[", quote(function() { }), 1), style = FALSE)
    Output
      [1] "`[`(function() { }, 1)"
    Code
      deparse_call(call("[", call("function", 1, 2), 1), style = FALSE)
    Output
      [1] "`[`(`function`(1, 2), 1)"

# curly braces

    Code
      deparse_call(call("{"), style = FALSE)
    Output
      [1] "{ }"
    Code
      deparse_call(call("{", 1, 2), style = FALSE)
    Output
      [1] "{\n  1\n  2\n}"
    Code
      deparse_call(call("{", 1, 2), one_liner = TRUE, style = FALSE)
    Output
      [1] "{1; 2}"
    Code
      deparse_call(call("{", 1, quote(expr = )), style = FALSE)
    Output
      [1] "`{`(1, )"

# Use lisp notation when the caller expr calls a control flow construct

    Code
      deparse_call(substitute(X(Y), list(X = quote(if (TRUE) { }), Y = 1)))
    Output
      `if`(TRUE, { })(1)
    Code
      deparse_call(substitute(X(Y), list(X = quote(while (TRUE) { }), Y = 1)))
    Output
      `while`(TRUE, { })(1)
    Code
      deparse_call(substitute(X(Y), list(X = quote(for (a in b) { }), Y = 1)))
    Output
      `for`(a, b, { })(1)
    Code
      deparse_call(substitute(X(Y), list(X = quote(repeat { }), Y = 1)))
    Output
      `repeat`({ })(1)

# Operator precedence is well handled

    Code
      deparse_call(str2lang("`^`(`+`(a, b), c)"))
    Output
      `^`(a + b, c)
    Code
      deparse_call(str2lang("`+`(`^`(a, b), c)"))
    Output
      a^b + c
    Code
      deparse_call(str2lang("`%in%`(`*`(a, b), c)"))
    Output
      `%in%`(a * b, c)
    Code
      deparse_call(str2lang("`*`(`%in%`(a, b), c)"))
    Output
      a %in% b * c
    Code
      deparse_call(str2lang("`+`(`+`(1, 2), 4)"))
    Output
      1 + 2 + 4
    Code
      deparse_call(str2lang("`-`(1+2)"))
    Output
      `-`(1 + 2)
    Code
      deparse_call(str2lang("`<-`(`<<-`(1, 2), 4)"))
    Output
      `<-`(1 <<- 2, 4)
    Code
      deparse_call(str2lang("`+`(x, y)(z)"))
    Output
      `+`(x, y)(z)
    Code
      deparse_call(quote(x <- a::b(y)))
    Output
      x <- a::b(y)
    Code
      deparse_call(quote(x <- a:::b(y)))
    Output
      x <- a:::b(y)
    Code
      deparse_call(quote(x <- a$b(y)))
    Output
      x <- a$b(y)
    Code
      deparse_call(quote(x <- a@b(y)))
    Output
      x <- a@b(y)
    Code
      deparse_call(quote(x <- a::b$c(y)))
    Output
      x <- a::b$c(y)
    Code
      deparse_call(str2lang("`^`(`^`(1, 2), 4)"))
    Output
      `^`(1^2, 4)
    Code
      deparse_call(str2lang("`^`(4, `^`(1, 2))"))
    Output
      4^1^2
    Code
      deparse_call(str2lang("`+`(4, `+`(1, 2))"))
    Output
      `+`(4, 1 + 2)
    Code
      deparse_call(substitute(X + Y, list(X = quote(repeat { }), Y = 1)))
    Output
      `+`(repeat { }, 1)
    Code
      deparse_call(substitute(X + Y, list(X = 1, Y = quote(repeat { }))))
    Output
      1 + repeat { }
    Code
      deparse_call(substitute(X ? Y, list(X = quote(repeat { }), Y = 1)))
    Output
      repeat { } ? 1
    Code
      deparse_call(substitute(X ? Y, list(X = 1, Y = quote(repeat { }))))
    Output
      1 ? repeat { }
    Code
      deparse_call(substitute(X$Y, list(X = quote(repeat { }), Y = 1)))
    Output
      `$`(repeat { }, 1)
    Code
      deparse_call(substitute(X$Y, list(X = 1, Y = quote(repeat { }))))
    Output
      `$`(1, repeat { })
    Code
      deparse_call(substitute(X(Y), list(X = quote(repeat { }), Y = 1)))
    Output
      `repeat`({ })(1)
    Code
      deparse_call(substitute(X[Y], list(X = quote(repeat { }), Y = 1)))
    Output
      `[`(repeat { }, 1)
    Code
      deparse_call(quote(+repeat { }))
    Output
      +repeat { }
    Code
      deparse_call(quote(+repeat { }))
    Output
      +repeat { }

