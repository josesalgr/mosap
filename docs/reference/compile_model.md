# Compile the optimization model stored in a Problem

Materializes the optimization model represented by a `Problem` object
without solving it. This is an advanced function mainly intended for
debugging, inspection, and explicit model preparation.

In standard workflows, users normally do not need to call this function,
because
[`solve()`](https://josesalgr.github.io/mosap/reference/solve.md)
compiles the model automatically when needed.

## Usage

``` r
compile_model(x, force = FALSE, ...)

# S3 method for class 'Problem'
compile_model(x, force = FALSE, ...)
```

## Arguments

- x:

  A `Problem` object.

- force:

  Logical. If `TRUE`, rebuild the model even if a current compiled model
  already exists.

- ...:

  Reserved for future extensions.

## Value

A `Problem` object with compiled model structures stored internally.
