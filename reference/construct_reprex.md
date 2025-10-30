# construct_reprex

`construct_reprex()` constructs all objects of the local environment, or
a caller environment `n` steps above. If `n > 0` the function call is
also included in a comment.

## Usage

``` r
construct_reprex(..., n = 0, include_dotted = TRUE)
```

## Arguments

- ...:

  Forwarded to
  [`construct_multi()`](https://cynkra.github.io/constructive/reference/construct.md)

- n:

  The number of steps to go up on the call stack

- include_dotted:

  Whether to include names starting with dots, this includes
  `.Random.seed` in the global environment and objects like `.Class` and
  `.Generic` in the execution environments of S3 methods.

## Value

An object of class 'constructive'.

## Details

`construct_reprex()` doesn't call the {reprex} package.
`construct_reprex()` builds reproducible data while the reprex package
build reproducible output once you have the data.

`construct_reprex()` wraps
[`construct_multi()`](https://cynkra.github.io/constructive/reference/construct.md)
and is thus able to construct unevaluated arguments using
[`delayedAssign()`](https://rdrr.io/r/base/delayedAssign.html). This
means we can construct reprexes for functions that use Non Standard
Evaluation.

A useful trick is to use `options(error = recover)` to be able to
inspect frames on error, and use `construct_reprex()` from there to
reproduce the data state.

`construct_reprex()` might fail to reproduce the output of functions
that refer to environments other than their caller environment. We
believe these are very rare and that the simplicity is worth the rounded
corners, but if you encounter these limitations please do open a ticket
on our issue tracker at `https://github.com/cynkra/constructive/` and we
might expand the feature.

## See also

[`construct_multi()`](https://cynkra.github.io/constructive/reference/construct.md)
