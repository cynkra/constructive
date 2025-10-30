# deparse_call

TODO: reorganize, put failing alternatives on top, then really wrong
stuff, then inaccuracies then readability.

[`constructive::deparse_call()`](https://cynkra.github.io/constructive/reference/deparse_call.md)
converts calls (language objects) to code. It is an alternative to
[`base::deparse()`](https://rdrr.io/r/base/deparse.html) or
[`rlang::expr_deparse()`](https://rlang.r-lib.org/reference/expr_print.html)
with a slightly different scope, and 3 main differences:

- [`deparse_call()`](https://cynkra.github.io/constructive/reference/deparse_call.md)
  faisl if the call is not syntactic (if it cannot be the output of
  `parse(text=x)[[1]]`), for instance if its AST contains elements that
  are not syntactic tokens

``` r
x <- call('+', c(1, 2))
base::deparse(x)
#> [1] "+c(1, 2)"
rlang::expr_deparse(x)
#> [1] "+<dbl: 1, 2>"
constructive::deparse_call(x)
#> Error in `constructive::deparse_call()`:
#> ! `call` must only be made of symbols and syntactic literals
#> Caused by error in `deparse_call_impl()` at constructive/R/deparse_call.R:105:5:
#> ! Found element of type 'double' and length '2':
#> c(1, 2)

# this is different
y <- quote(+c(1, 2))
x[[2]]
#> [1] 1 2
y[[2]]
#> c(1, 2)
```

- [`deparse_call()`](https://cynkra.github.io/constructive/reference/deparse_call.md)
  never makes compromises to make code more readable at the expense of
  accuracy.

``` r
x <- quote(`*`(a + b, c))
base::deparse(x)
#> [1] "(a + b) * c"
rlang::expr_deparse(x)
#> [1] "(a + b) * c"
constructive::deparse_call(x)
#> `*`(a + b, c)

y <- quote((a + b) * c)
base::deparse(y)
#> [1] "(a + b) * c"
rlang::expr_deparse(y)
#> [1] "(a + b) * c"
constructive::deparse_call(y)
#> (a + b) * c

# x and y are different, parentheses are code!
x[[2]]
#> a + b
y[[2]]
#> (a + b)
```

- [`deparse_call()`](https://cynkra.github.io/constructive/reference/deparse_call.md)
  handles many more contrived cases. It strives to provide an accurate
  syntactic representation for every possible syntactic language object,
  however unprobable or unpractical they might be.

``` r
x <- call("[")
base::deparse(x)
#> [1] "NULL[]"
rlang::expr_deparse(x)
#> [1] "NULL[]"
constructive::deparse_call(x)
#> `[`()
```

## `deparse_call()` is more accurate

We present more differences below, where at least one of the
alternatives is not deparsing faithfully.

[TABLE]

## `deparse_call()` is clearer

In the following
[`base::deparse()`](https://rdrr.io/r/base/deparse.html) and
[`rlang::expr_deparse()`](https://rlang.r-lib.org/reference/expr_print.html)
are not wrong, but
[`constructive::deparse_call()`](https://cynkra.github.io/constructive/reference/deparse_call.md)
is clearer.

|                                                                 | constructive::deparse_call()      | base::deparse()              | rlang::expr_deparse()        |
|-----------------------------------------------------------------|-----------------------------------|------------------------------|------------------------------|
| Simple quotes make strings that use double quotes more readable | `'"oh" "hey" "there"'`            | `"\"oh\" \"hey\" \"there\""` | `"\"oh\" \"hey\" \"there\""` |
| Raw strings make more complex strings more readable             | `r"["oh"\'hey'\"there"]"`         | `"\"oh\"\\'hey'\\\"there\""` | `"\"oh\"\\'hey'\\\"there\""` |
| Homoglyphs are dangerous, we can use the `\U{XX}` notation      | `"\U{410} \U{A0} A"`              | `"А   A"`                    | `"А   A"`                    |
| For symbols we need the `\xXX` notation                         | `` c(`\xd0\x90`, "\U{A0}" = 1) `` | `` c(А, ` ` = 1) ``          | `` c(А, ` ` = 1) ``          |
| Emojis depend on font so are ambiguous                          | `"\U{1F436}"`                     | `"🐶"`                       | `"🐶"`                       |

## `deparse_call()` fails rather than making things up

``` r
x <- call("(", -1)
base::deparse(x)
#> [1] "(-1)"
rlang::expr_deparse(x)
#> [1] "(-1)"
constructive::deparse_call(x)
#> Error in `constructive::deparse_call()`:
#> ! `call` must only be made of symbols and syntactic literals
#> Caused by error in `deparse_call_impl()` at constructive/R/deparse_call.R:105:5:
#> ! Found element of type 'double' and length '1':
#> -1

# this is different! `-` is code!
y <- quote((-1))
base::deparse(y)
#> [1] "(-1)"
rlang::expr_deparse(y)
#> [1] "(-1)"
constructive::deparse_call(y)
#> (-1)


x <- call("fun", quote(expr = ))
base::deparse(x)
#> [1] "fun()"
rlang::expr_deparse(x)
#> [1] "fun()"
constructive::deparse_call(x) # this is wrong!
#> Error in `constructive::deparse_call()`:
#> ! `call` must only be made of symbols and syntactic literals
#> Caused by error in `deparse_call_impl()` at constructive/R/deparse_call.R:49:3:
#> ! Found empty symbol used as sole argument of a function:
#> as.call(list(quote(fun), quote(expr = )))

# no agument and 1 missing argument is not the same!
y <- call("fun")
base::deparse(y)
#> [1] "fun()"
rlang::expr_deparse(y)
#> [1] "fun()"
constructive::deparse_call(y)
#> fun()

x <- call("!", quote(expr = ))
base::deparse(x)
#> [1] "!"
rlang::expr_deparse(x)
#> [1] "!"
constructive::deparse_call(x)
#> Error in `constructive::deparse_call()`:
#> ! `call` must only be made of symbols and syntactic literals
#> Caused by error in `deparse_call_impl()` at constructive/R/deparse_call.R:49:3:
#> ! Found empty symbol used as sole argument of a function:
#> as.call(list(quote(`!`), quote(expr = )))
```
