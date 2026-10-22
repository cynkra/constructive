## Notes about version 1.4.0

This resubmission addresses the archival of 1.3.0. Its tests failed against
ellmer 0.5.0, which deprecated the `.additional_properties` argument of
`type_object()`; we don't emit that argument anymore.

Also a large batch of new features, mostly support for the classes of 14 more
packages, and a declared dependency on R >= 4.0.0, which our imports rlang and
waldo have required for a while.

Checked on the R versions from 4.2 to devel, on Linux, macOS and Windows, and
with suggested packages removed in turn. All green.

About the incoming pretest NOTEs of the first submission:

* The HTML manual validation problem in `opts_collector.Rd` is fixed.
* The Author field differed because the tarball was built with R 4.4, which
  formats ORCID and ROR comments differently, it is now built with R 4.6.
* The ROR ID 0335t7e62 (cynkra GmbH) is valid, see
  https://api.ror.org/v2/organizations/0335t7e62 .

## Notes about version 1.3.0

1.2.0 coudn't be installed on some systems, the C code needed fixing. + a few minor features and fixes.

## Notes about version 1.2.0

Fixes and new features, compiled code also had to be adapted for R 4.5. All checks green AFAICT.

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

