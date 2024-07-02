## R CMD check results

0 errors | 0 warnings | 1 note

## Adressing Benjamin Altmann's comment for version 0.0.1 (2021-06-05)

> \dontrun{} should only be used if the example really cannot be executed 
(e.g. because of missing additional software, missing API keys, ...) by 
the user [...]

In my case I believe this is justified, if I use `donttest` or  I unprotect my example I get:

LaTeX errors found:
  ! LaTeX Error: Unicode character ^^[ (U+001B)
                 not set up for use with LaTeX.
                 
I used `\dontrun{}` because `construct_diff()` uses the viewer several times and running `example(construct_diff)` doesn't make sense.

