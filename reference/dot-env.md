# Fetch environment from memory address

This is designed to be used in constructed output. The `parents` and
`...` arguments are not processed and only used to display additional
information. If used on an improper memory address it will either fail
(most likely) or the output will be erratic.

## Usage

``` r
.env(address, parents = NULL, ...)
```

## Arguments

- address:

  Memory address of the environment

- parents, ...:

  ignored

## Value

The environment that the memory address points to.
