# Build optimization model from Data

Materializes (builds) the optimization model using the current state of
the `Data` object: prepared data tables, stored objective settings, and
stored constraints (e.g., targets).

## Usage

``` r
.pa_build_model(x)
```

## Arguments

- x:

  Data object (class "Data") created with
  inputData()/inputDataSpatial().

## Value

Updated `Data` object with model pointer and model snapshot.
