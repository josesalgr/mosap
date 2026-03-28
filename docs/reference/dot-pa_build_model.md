# Build optimization model from Problem

Materializes (builds) the optimization model using the current state of
the `Problem` object: prepared data tables, stored objective settings,
and stored constraints (e.g., targets).

## Usage

``` r
.pa_build_model(x)
```

## Arguments

- x:

  Problem object (class "Problem") created with input_data().

## Value

Updated `Problem` object with model pointer and model snapshot.
