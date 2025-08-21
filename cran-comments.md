## Notes about version 1.2.0

Fixes and new features, the core of the package stays the same. All checks green AFAICT.

## Notes about version 1.1.0

A few fixes and new features but no deep changes, all checks green AFAICT.

## Notes about version 1.0.1

This contains a few fixes and improvement described in NEWS. Apologies for
the short delay between the major release and the patch, I'm presenting
the package next week at useR! 2024 and would like it to be in good shape.

## Adressing Benjamin Altmann's comment for version 0.0.1 (2021-06-05)

> \dontrun{} should only be used if the example really cannot be executed 
(e.g. because of missing additional software, missing API keys, ...) by 
the user [...]

In my case I believe this is justified, if I use `donttest` or  I unprotect my example I get:

LaTeX errors found:
  ! LaTeX Error: Unicode character ^^[ (U+001B)
                 not set up for use with LaTeX.
                 
I used `\dontrun{}` because `construct_diff()` uses the viewer several times and running `example(construct_diff)` doesn't make sense.

