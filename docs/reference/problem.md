# Configure mathematical model (legacy-compatible preset)

Legacy-compatible helper that stores a model preset inside the `Data`
object. In the new pipeline, the MILP is built inside
[`solve()`](https://josesalgr.github.io/mosap/reference/solve.md). This
function does NOT create a C++ model pointer; it only configures
`x$data$model_args` for
[`.pa_build_model()`](https://josesalgr.github.io/mosap/reference/dot-pa_build_model.md).

## Usage

``` r
problem(
  x,
  model_type = c("minimizeCosts", "maximizeBenefits"),
  budget = 0,
  blm = 0,
  curve = 1,
  segments = 3
)
```

## Arguments

- x:

  Data object (class "Data") created with
  inputData()/inputDataSpatial().

- model_type:

  character. "minimizeCosts" or "maximizeBenefits".

- budget:

  numeric. Stored in model_args (compat/printing).

- blm:

  numeric. Stored in model_args (compat/printing).

- curve:

  integer. Stored in model_args (compat/printing).

- segments:

  integer. Stored in model_args (compat/printing).

## Value

Updated `Data` object with preset stored in `x$data$model_args`.
