# Combine errors

Exported for custom constructor design. This function allows combining
independent checks so information is given about all failing checks
rather than the first one. All parameters except `...` are forwarded to
[`rlang::abort()`](https://rlang.r-lib.org/reference/abort.html)

## Usage

``` r
.cstr_combine_errors(
  ...,
  class = NULL,
  call,
  header = NULL,
  body = NULL,
  footer = NULL,
  trace = NULL,
  parent = NULL,
  use_cli_format = NULL,
  .internal = FALSE,
  .file = NULL,
  .frame = parent.frame(),
  .trace_bottom = NULL
)
```

## Arguments

- ...:

  check expressions

- class:

  Subclass of the condition.

- call:

  The execution environment of a currently running function, e.g.
  `call = caller_env()`. The corresponding function call is retrieved
  and mentioned in error messages as the source of the error.

  You only need to supply `call` when throwing a condition from a helper
  function which wouldn't be relevant to mention in the message.

  Can also be `NULL` or a [defused function
  call](https://rlang.r-lib.org/reference/topic-defuse.html) to
  respectively not display any call or hard-code a code to display.

  For more information about error calls, see [Including function calls
  in error
  messages](https://rlang.r-lib.org/reference/topic-error-call.html).

- header:

  An optional header to precede the errors

- body, footer:

  Additional bullets.

- trace:

  A `trace` object created by
  [`trace_back()`](https://rlang.r-lib.org/reference/trace_back.html).

- parent:

  Supply `parent` when you rethrow an error from a condition handler
  (e.g. with
  [`try_fetch()`](https://rlang.r-lib.org/reference/try_fetch.html)).

  - If `parent` is a condition object, a *chained error* is created,
    which is useful when you want to enhance an error with more details,
    while still retaining the original information.

  - If `parent` is `NA`, it indicates an unchained rethrow, which is
    useful when you want to take ownership over an error and rethrow it
    with a custom message that better fits the surrounding context.

    Technically, supplying `NA` lets `abort()` know it is called from a
    condition handler. This helps it create simpler backtraces where the
    condition handling context is hidden by default.

  For more information about error calls, see [Including contextual
  information with error
  chains](https://rlang.r-lib.org/reference/topic-error-chaining.html).

- use_cli_format:

  Whether to format `message` lazily using [cli](https://cli.r-lib.org/)
  if available. This results in prettier and more accurate formatting of
  messages. See
  [`local_use_cli()`](https://rlang.r-lib.org/reference/local_use_cli.html)
  to set this condition field by default in your package namespace.

  If set to `TRUE`, `message` should be a character vector of individual
  and unformatted lines. Any newline character `"\\n"` already present
  in `message` is reformatted by cli's paragraph formatter. See
  [Formatting messages with
  cli](https://rlang.r-lib.org/reference/topic-condition-formatting.html).

- .internal:

  If `TRUE`, a footer bullet is added to `message` to let the user know
  that the error is internal and that they should report it to the
  package authors. This argument is incompatible with `footer`.

- .file:

  A connection or a string specifying where to print the message. The
  default depends on the context, see the `stdout` vs `stderr` section.

- .frame:

  The throwing context. Used as default for `.trace_bottom`, and to
  determine the internal package to mention in internal errors when
  `.internal` is `TRUE`.

- .trace_bottom:

  Used in the display of simplified backtraces as the last relevant call
  frame to show. This way, the irrelevant parts of backtraces
  corresponding to condition handling
  ([`tryCatch()`](https://rdrr.io/r/base/conditions.html),
  [`try_fetch()`](https://rlang.r-lib.org/reference/try_fetch.html),
  `abort()`, etc.) are hidden by default. Defaults to `call` if it is an
  environment, or `.frame` otherwise. Without effect if `trace` is
  supplied.

## Value

Returns `NULL` invisibly, called for side effects.
