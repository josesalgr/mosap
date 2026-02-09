# Create and solve multi-actions planning problems

Create and solve a multi-actions planning problem in a single call. This
is a convenience wrapper around:

    inputData() -> (optional: add_spatial_* relations) -> solve()

## Usage

``` r
mosap(...)
```

## Arguments

- ...:

  Arguments inherited from
  [`inputData()`](https://josesalgr.github.io/mosap/reference/inputData.md)
  and [`solve()`](https://josesalgr.github.io/mosap/reference/solve.md).
  In addition, this wrapper supports `boundary` as described above.

## Value

An object of class
[solution](https://josesalgr.github.io/mosap/reference/solution-class.md).

## Details

**No
[`problem()`](https://josesalgr.github.io/mosap/reference/problem.md):**
This wrapper does not call
[`problem()`](https://josesalgr.github.io/mosap/reference/problem.md).
The optimization model is expected to be built and solved by
[`solve()`](https://josesalgr.github.io/mosap/reference/solve.md) from
the information stored in the `Data` object (objectives, targets,
relations, etc.).

**Spatial relations / boundary:**

- This function does not pass `boundary` into
  [`inputData()`](https://josesalgr.github.io/mosap/reference/inputData.md).

- If you provide `boundary`, it is interpreted as a request to register
  a spatial relation after
  [`inputData()`](https://josesalgr.github.io/mosap/reference/inputData.md)
  and before
  [`solve()`](https://josesalgr.github.io/mosap/reference/solve.md).

- Supported `boundary` values:

  - a `data.frame` boundary table (e.g., columns `id1,id2,boundary` or
    `pu1,pu2,weight`), registered via
    [`add_spatial_boundary()`](https://josesalgr.github.io/mosap/reference/add_spatial_boundary.md).

  - `"auto"` to derive boundary-length relations from `x$data$pu_sf`
    (requires sf and that
    [`inputData()`](https://josesalgr.github.io/mosap/reference/inputData.md)
    stored `pu_sf`).

## Examples

``` r
# \donttest{
set.seed(14)

data(sim_pu_data, sim_features_data, sim_dist_features_data,
     sim_threats_data, sim_dist_threats_data, sim_sensitivity_data,
     sim_boundary_data)

## One-shot solve (tabular), registering a boundary relation from a table
s <- mosap(
  pu = sim_pu_data,
  features = sim_features_data,
  dist_features = sim_dist_features_data,
  threats = sim_threats_data,
  dist_threats = sim_dist_threats_data,
  sensitivity = sim_sensitivity_data,
  boundary = sim_boundary_data,
  model_type = "minimizeCosts",
  time_limit = 50,
  output_file = FALSE,
  cores = 2
)
#> Error in mosap(pu = sim_pu_data, features = sim_features_data, dist_features = sim_dist_features_data,     threats = sim_threats_data, dist_threats = sim_dist_threats_data,     sensitivity = sim_sensitivity_data, boundary = sim_boundary_data,     model_type = "minimizeCosts", time_limit = 50, output_file = FALSE,     cores = 2): The following params are not defined in this function: pu features dist_features threats dist_threats sensitivity boundary time_limit output_file cores
print(s)
#> Error: object 's' not found

## Spatial example (conceptual): boundary derived from PU polygons stored by inputData()
## s2 <- mosap(
##   pu = pu_sf, features = feat_r, cost = cost_r,
##   boundary = "auto",
##   model_type = "minimizeFragmentation",
##   time_limit = 60
## )
# }
```
