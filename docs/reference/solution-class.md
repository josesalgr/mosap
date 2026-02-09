# Solution class

This class is used to represent the solution of the MIP (Mixed-Integer
Programming) model. This includes several methods to obtain information
about both the optimization process and the solution associated with the
planning units and actions. It is created using the
[`solve()`](https://josesalgr.github.io/mosap/reference/solve.md)
function.

## Value

No return value.

## Fields

- \$data:

  `list`. Object containing data on the results of the optimization
  process.

## Methods

- print():

  Print basic information of the model solution.

- show():

  Call print method.

## Examples

``` r
# \donttest{
# set seed for reproducibility
set.seed(14)

## Load data
data(sim_pu_data, sim_features_data, sim_dist_features_data,
sim_threats_data, sim_dist_threats_data, sim_sensitivity_data,
sim_boundary_data)

## Create data instance
problem_data <- inputData(
  pu = sim_pu_data, features = sim_features_data, dist_features = sim_dist_features_data,
  threats = sim_threats_data, dist_threats = sim_dist_threats_data,
  sensitivity = sim_sensitivity_data, boundary = sim_boundary_data
)
#> Error in .pa_inputData_tabular_impl(pu = pu, features = features, dist_features = dist_features,     boundary = NULL, ...): formal argument "boundary" matched by multiple actual arguments

## Create optimization model
problem_model <- problem(x = problem_data, blm = 1)
#> Error: object 'problem_data' not found

## Solve the optimization model
s <- solve(a = problem_model, time_limit = 5, output_file = FALSE, cores = 2)
#> Error: object 'problem_model' not found

## Use class methods

s$print()
#> Error: object 's' not found
# }
```
