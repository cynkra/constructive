## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Answering comments from Benjamin Altmann 2021-06-05

> Please add \value to .Rd files regarding exported methods and explain 
the functions results in the documentation [...]

Added requested \value to construct_signature.Rd, dot-cstr_match_constructor.Rd,
  dot-cstr_register_constructors.Rd, dot-env.Rd, dot-xptr.Rd

> \dontrun{} should only be used if the example really cannot be executed 
(e.g. because of missing additional software, missing API keys, ...) by 
the user [...]

In my case I believe this is justify, if I use `donttest` or  I unprotect my example I get:

LaTeX errors found:
  ! LaTeX Error: Unicode character ^^[ (U+001B)
                 not set up for use with LaTeX.
                 
I used `\dontrun{}` because `construct_diff()` uses the viewer several times and running `example(construct_diff)`
doesn't make sense.

> In construct_diff.Rd in the commented codelines you write 'differemt' 
instead of 'different'.

Fixed the typo

> You are using installed.packages() in your code. [...]

We now use `rlang::is_installed()` instead.