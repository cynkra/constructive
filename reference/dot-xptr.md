# Build a pointer from a memory address

Base R doesn't provide utilities to build or manipulate external
pointers (objects of type "externalptr"), so we provide our own. Objects
defined with `.xptr()` are not stable across sessions,

## Usage

``` r
.xptr(address)
```

## Arguments

- address:

  Memory address

## Value

The external pointer (type "externalptr") that the memory address points
to.
