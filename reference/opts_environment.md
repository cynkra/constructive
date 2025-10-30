# Constructive options for type 'environment'

Environments use reference semantics, they cannot be copied. An attempt
to copy an environment would indeed yield a different environment and
`identical(env, copy)` would be `FALSE`.  
Moreover most environments have a parent (exceptions are
[`emptyenv()`](https://rdrr.io/r/base/environment.html) and some rare
cases where the parent is `NULL`) and thus to copy the environment we'd
have to have a way to point to the parent, or copy it too.  
For this reason environments are constructive's cryptonite. They make
some objects impossible to reproduce exactly. And since every function
or formula has one they're hard to avoid.  

## Usage

``` r
opts_environment(
  constructor = c(".env", "list2env", "as.environment", "new.env", "topenv",
    "new_environment", "predefine"),
  ...,
  recurse = FALSE
)
```

## Arguments

- constructor:

  String. Name of the function used to construct the environment, see
  **Constructors** section.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

- recurse:

  Boolean. Only considered if `constructor` is `"list2env"` or
  `"new_environment"`. Whether to attempt to recreate all parent
  environments until a known environment is found, if `FALSE` (the
  default) we will use
  [`topenv()`](https://rdrr.io/r/base/ns-topenv.html) to find a known
  ancestor to set as the parent.

## Value

An object of class
\<constructive_options/constructive_options_environment\>

## Details

In some case we can build code that points to a specific environment,
namely:

- `.GlobalEnv`, `.BaseNamespaceEnv`,
  [`baseenv()`](https://rdrr.io/r/base/environment.html) and
  [`emptyenv()`](https://rdrr.io/r/base/environment.html) are used to
  construct the global environment, the base namespace, the base package
  environment and the empty environment

- Namespaces are constructed using `asNamespace("pkg")`

- Package environments are constructed using
  `as.environment("package:pkg")`

- "imports" environments are constructed with
  `parent.env(asNamespace("pkg"))`

- "lazydata" environments are constructed with
  `getNamespaceInfo("pkg", "lazydata")`

By default For other environments we use constructive's function
[`constructive::.env()`](https://cynkra.github.io/constructive/reference/dot-env.md),
it fetches the environment from its memory address and provides as
additional information the sequence of parents until we reach a special
environment (those enumerated above). The advantage of this approach is
that it's readable and that the object is accurately reproduced. The
inconvenient is that it's not stable between sessions. If an environment
has a `NULL` parent it's always constructed with
[`constructive::.env()`](https://cynkra.github.io/constructive/reference/dot-env.md),
whatever the choice of the constructor.

Often however we wish to be able to reproduce from scratch a similar
environment, so that we might run the constructed code later in a new
session. We offer different different options to do this, with different
trade-offs regarding accuracy and verbosity.

{constructive} will not signal any difference if it can reproduce an
equivalent environment, defined as containing the same values and having
a same or equivalent parent.  

See also the `ignore_function_env` argument in
[`?compare_options`](https://cynkra.github.io/constructive/reference/compare_options.md),
which disables the check of environments of function.

## Constructors

We might set the `constructor` argument to:

- `".env"` (default): use
  [`constructive::.env()`](https://cynkra.github.io/constructive/reference/dot-env.md)
  to construct the environment from its memory address.

&nbsp;

- `"list2env"`: We construct the environment as a list then use
  [`base::list2env()`](https://rdrr.io/r/base/list2env.html) to convert
  it to an environment and assign it a parent. By default we use as a
  parent the first special environment we find when going through
  ancestors, so we can print code that doesn't use
  [`.env()`](https://cynkra.github.io/constructive/reference/dot-env.md).
  If `recurse` is `TRUE` the parent will be built recursively so all
  ancestors will be created until we meet a known environment, this
  might be verbose and will fail if environments are nested too deep or
  have a circular relationship. If the environment is empty we use
  `new.env(parent=)` for a more economic syntax.

- `"new_environment"` : Similar to the above, but using
  [`rlang::new_environment()`](https://rlang.r-lib.org/reference/env.html).

- `"new.env"` : All environments will be recreated with the code
  `"base::new.env()"`, without argument, effectively creating an empty
  environment child of the local (often global) environment. This is
  enough in cases where the environment doesn't matter (or matters as
  long as it inherits from the local environment), as is often the case
  with formulas. `recurse` is ignored.

- `"as.environment"` : we attempt to construct the environment as a list
  and use
  [`base::as.environment()`](https://rdrr.io/r/base/as.environment.html)
  on top of it, as in `as.environment(list(a=1, b=2))`, it will contain
  the same variables as the original environment but the parent will be
  the [`emptyenv()`](https://rdrr.io/r/base/environment.html). `recurse`
  is ignored.

- `"topenv"` : we construct `base::topenv(x)`, see
  [`?topenv`](https://rdrr.io/r/base/ns-topenv.html). `recurse` is
  ignored. This is the most accurate we can be when constructing only
  special environments.

- `"predefine"` : Building environments from scratch using the above
  methods can be verbose, sometimes redundant and sometimes even
  impossible due to circularity (e.g. an environment referencing
  itself). With `"predefine"` we define the environments and their
  content above the object returning call, using placeholder names
  `..env.1..`, `..env.2..` etc. The caveat is that the created code
  won't be a single call and will create objects in the workspace.
  `recurse` is ignored.
