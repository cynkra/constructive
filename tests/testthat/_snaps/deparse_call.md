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
      [1] "1[]"
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
      [1] "1[[]]"
    Code
      deparse_call(call("[["), style = FALSE)
    Output
      [1] "`[[`()"
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

