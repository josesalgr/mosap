# Simulated multi-action planning data

Simulated data for making prioritizations.

- `sim_pu_data`:

  Planning units are represented as tabular data.

- `sim_features_data`:

  Features are represented as tabular data.

- `sim_dist_features_data`:

  The simulated distribution of four features.

- `sim_threats_data`:

  Threats are represented as tabular data.

- `sim_dist_threats_data`:

  The simulated threats of two threats.

- `sim_sensitivity_data`:

  Sensitivity of features to threats as tabular data.

- `sim_boundary_data`:

  Boundary data between one hundred planning units.

## Usage

``` r
data(sim_pu_data)

data(sim_features_data)

data(sim_dist_features_data)

data(sim_threats_data)

data(sim_dist_threats_data)

data(sim_sensitivity_data)

data(sim_boundary_data)
```

## Format

- sim_pu_data:

  [data.frame](https://rdrr.io/r/base/data.frame.html) object.

- sim_features_data:

  [data.frame](https://rdrr.io/r/base/data.frame.html) object.

- sim_dist_features_data:

  [data.frame](https://rdrr.io/r/base/data.frame.html) object.

- sim_threats_data:

  [data.frame](https://rdrr.io/r/base/data.frame.html) object.

- sim_dist_threats_data:

  [data.frame](https://rdrr.io/r/base/data.frame.html) object.

- sim_sensitivity_data:

  [data.frame](https://rdrr.io/r/base/data.frame.html) object.

- sim_boundary_data:

  [data.frame](https://rdrr.io/r/base/data.frame.html) object.

## Examples

``` r
if (FALSE) { # \dontrun{
# load data
data(sim_pu_data, sim_features_data, sim_dist_features_data,
sim_threats_data, sim_dist_threats_data, sim_sensitivity_data,
sim_boundary_data)

# plot examples
library(raster)
r <- raster::raster(ncol=10, nrow=10, xmn=0, xmx=10, ymn=0, ymx=10)

# plot cost of pu's
values(r) <- sim_pu_data$monitoring_cost
plot(r)

# plot feature distribution of feature 1
features <- tidyr::spread(data = sim_dist_features_data, key = feature, value = amount, fill = 0)
values(r) <- features$'1'
plot(r)
} # }
```
