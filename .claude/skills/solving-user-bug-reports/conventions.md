# constructive code and test conventions

Brief for any code change in constructive. Read the files cited here on `origin/main`, the local
checkout may be behind.

## Why we avoid S3 dispatch, and the shim system

constructive describes the *true structure* of an object: its type, data and attributes. Generics like
`as.character()`, `is.na()`, `length()`, `[`, `[[`, `==`, `format()` are S3 or internal generics: on a
classed object they run whatever method the class (or any loaded package) registered. Inside a
constructor that is almost always accidental and gives wrong results:
- the method reports a *view* of the object, not its data (`length()` of a POSIXlt, `[` on a data.table,
  `as.character()` of a factor, `==` on a class with an `Ops` method);
- the result depends on which packages are loaded, so the bug only shows for some users;
- the object may be corrupted (that's when users need constructive most), and methods error on it.

So in package code, base generics are **masked by shims** in `R/bypass.R`. Each shim calls the base
function on `unclass(x)`, which gives the default behaviour whatever the class:

```r
as.character <- function(x, ...) base::as.character(unclass(x), ...)
is.na <- function(x) base::is.na(unclass(x))
```

Shimmed today: `c`, `unlist`, `lapply`, `sapply`, `vapply`, `as.character`, `as.double`/`as.numeric`,
`as.integer`, `as.logical`, `as.complex`, `as.raw`, `is.na`, `is.nan`, `is.finite`, `is.infinite`,
`anyNA`, `is.numeric`, `length`, `lengths`, `dim`, `dim<-`, `dimnames`, `dimnames<-`, `names`, `names<-`,
`$`, `[`, `[[`, `[<-`, `[[<-`, `$<-`, `==`, `!=`, `<`, `>`, `<=`, `>=`, `/`. Check `R/bypass.R` for the
current list. Consequences:

- Write plain `length(x)`, `x[i]`, `x == y`, `is.na(x)` in package code: they already bypass dispatch.
- **Don't inline `unclass()`** to dodge dispatch of a generic that isn't shimmed. Add a shim to
  `R/bypass.R` in the right section, and restore the base function in `tests/testthat/setup.R`
  (`fun <- base::fun`): tests run with the package namespace as parent and must get normal behaviour.
  Adding a shim changes every call site: run the full suite, failures reveal places that relied on dispatch.
- **When a specific behaviour is wanted, call the method by name**: `as.character.factor(x)`,
  `as.double.POSIXlt(x)`, `format.Date(x)`, `bit64::is.na.integer64(x)`. It states intent and can't be
  hijacked by another class.
- Use `base::generic(x)` (which does dispatch, including to other packages' methods) only when the class
  legitimately varies or the method isn't exported (e.g. `as.character.xml_document`), with a comment
  saying why.
- `as.numeric` is `as.double`: methods and shims are written for `as.double`. Internal generics only
  dispatch when `is.object(x)`. `attributes(x) <- NULL` is not equivalent to `unclass()` + default.
- Older code still has some unprotected generic calls (e.g. `format(x)` in `R/s3-Date.R`); don't copy
  them into new code, and don't refactor unrelated ones in a bug fix (mention them instead).
- Don't introduce new S3 generics for internal helpers. The only dispatch in constructive is the
  deliberate one below: `.cstr_construct()` on the object's class, then on the chosen constructor.

## Constructor anatomy

One file per class, `R/s3-<class>.R` (`s4.R`, `s7-*.R`, `s3-<pkg>-<class>.R` for some packages).
Pattern (see `R/s3-Date.R`, `R/s3-sfc.R`, and `vignettes/extend-constructive.Rmd`):

```r
#' Constructive options for class 'foo'
#' ... (one bullet per constructor, "next" last; documents fallbacks)
#' @export
opts_foo <- function(constructor = c("foo", "next"), ...) {
  .cstr_options("foo", constructor = constructor[[1]], ...)    # no match.arg(): constructors can be added outside
}

#' @export
#' @method .cstr_construct foo
.cstr_construct.foo <- function(x, ...) {
  opts <- list(...)$opts$foo %||% opts_foo()
  if (is_corrupted_foo(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.foo", structure(NA, class = opts$constructor))
}

is_corrupted_foo <- function(x) {
  # type/attributes/structure the idiomatic constructor relies on
}

#' @export
#' @method .cstr_construct.foo foo
.cstr_construct.foo.foo <- function(x, ...) {          # args are exactly x and ...
  args <- ...                                          # data as plain R values
  code <- .cstr_apply(args, "pkg::foo", ...)           # recursive construction of args
  repair_attributes_foo(x, code, ...)                  # always last
}

repair_attributes_foo <- function(x, code, ...) {
  .cstr_repair_attributes(x, code, ..., idiomatic_class = "foo", ignore = <attrs set by foo()>)
}
```

- Options are read with `list(...)$opts$<class> %||% opts_<class>()`. Pass `...` through everywhere.
- A constructor that can't represent the data (Inf, NA, non-integer...) falls back to another constructor
  of the same class by calling it directly (`return(.cstr_construct.Date.as.Date.numeric(x, ...))`).
- An object that doesn't meet the class's expectations is "corrupted": tighten `is_corrupted_<class>()` so
  it falls through `NextMethod()`; constructive must never error on a corrupted object.
- Generated code calls functions of other packages namespaced (`"sf::st_sfc"`, `"vctrs::new_date"`); the
  package must be in Suggests; use `pkg::fun` in our own code too.
- Version-dependent behaviour: `with_versions(ggplot2 < "4.0.0")` (internal helper in `R/utils.R`,
  accepts `R` and package names). Use it in code and tests rather than `packageVersion()` comparisons.
- Prefer the minimal DRY change: transform the data and feed it to existing constructors / `.cstr_apply()`;
  don't build code strings with ad hoc `paste()`/layout logic. Mention a trade-off if the output is less
  fancy than the issue's sketch.
- Roxygen: `@export` + `@method` tags on each method, regenerate with `devtools::document()`.
  A newly supported class is also registered in `all_classes` in `R/construct-helpers.R` (follow a recent
  "Support <pkg> classes" PR, e.g. #715).
- Comments are sparse and explain *why*; style matches the file (2-space indent, `<-`, double quotes).

## Tests

- File `tests/testthat/test-s3-<class>.R` (or the matching `test-<topic>.R`), add to the existing file.
- Form: `test_that("<class>", { expect_snapshot({ construct(...) ; construct(..., opts_<class>("x")) }) })`.
  `expect_snapshot` is redefined in `tests/testthat/setup.R` (normalizes `%>%`/`|>` and `_` placeholders):
  use it as is. Snapshots live in `tests/testthat/_snaps/<file>.md`, generated by running the test, never
  written by hand. Read the snapshot: it must show the correct idiomatic code.
- Regression test: the minimal object from the issue, added to the class's existing block or a new block
  named after the behaviour (no issue-number tags in tests; the link lives in the commit and PR). Test the
  default constructor and any `opts_*()` variant the bug touches; for corruption bugs, a corrupted object.
- Snapshot tests only run with `NOT_CRAN=true`.
- `::` is redefined in setup.R to skip when the package isn't installed, but call
  `skip_if_not_installed("<pkg>")` explicitly at the start of the test, and **always before**
  `with_versions(<pkg> ...)` (it errors when the package is missing).
- Version/platform-dependent output: split into separate `test_that()` blocks whose names say the
  condition, each with its skip, e.g.
  `test_that("tibbles with NULL names, vctrs >= 0.7.2", { skip_if_not_installed("vctrs"); skip_if(with_versions(vctrs < "0.7.2")); ... })`.
  Other helpers: `skip_if_tz_unavailable("CET")` (`helper-timezone.R`), `skip_if(!l10n_info()$\`UTF-8\`)`,
  `withr::local_timezone()` / `withr::local_options()` for determinism.
- Never commit `.new.md` files or snapshots produced in an environment that differs from the reference
  (wrong locale, missing timezone, different package version); explain them instead.

## Running tests locally (this machine)

```bash
# one file
LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8 NOT_CRAN=true Rscript -e 'library(anytime); devtools::test("<pkg dir>", filter = "s3-Date")'
# full suite, run in background (several minutes): expect 0 failures on a clean main
LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8 NOT_CRAN=true Rscript -e 'library(anytime); devtools::test("<pkg dir>")'
```

The C locale gives ~11 spurious failures; loading anytime first avoids a tsibble/anytime crash when
testthat's compiled code loads first.
