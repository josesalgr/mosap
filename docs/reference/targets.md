# Targets API

Functions to define feature-level targets for a planning problem.
Targets are stored in `x$data$targets` and later translated into
mathematical constraints when the optimization model is built.

The API supports three target types:

- **Conservation**:

  Ensure baseline representation of selected planning units reaches a
  threshold.

- **Recovery**:

  Ensure action-driven improvements (deltas) reach a threshold.

- **Mixed total**:

  Ensure baseline + action deltas together reach a single threshold.

Each target is stored as a row with at least: `feature`, `type`,
`sense`, `target_unit`, `target_raw`, `basis_total`, `target_value`, and
optional metadata such as `label` and `created_at`.

## Details

**Targets format.** The `targets` argument can be provided in multiple
equivalent ways (as implemented by `.pa_parse_targets()`), typically
including:

- a single numeric value recycled to all features,

- a numeric vector aligned to the feature order,

- a named numeric vector where names identify features,

- a `data.frame` with feature identifiers and target values.

Features may be identified by numeric `id` and/or by a feature name
column if supported by `.pa_parse_targets()`.

**Absolute vs relative targets.** Absolute targets set `target_value`
directly from the provided `targets`. Relative targets treat `targets`
as proportions in \\\[0,1\]\\ and convert them to absolute thresholds by
multiplying by a feature-specific basis:

- Conservation relative targets use baseline totals (via
  `.pa_feature_totals()`).

- Recovery relative targets use either potential improvement (via
  `.pa_feature_potential()`) or baseline totals, depending on
  `relative_basis`.

- Mixed-total relative targets use baseline totals (via
  `.pa_feature_totals()`).

The chosen basis is stored in `basis_total`, and the resulting absolute
threshold is stored in `target_value`.

**Mutual exclusivity for mixed-total targets.** Mixed-total targets
represent a single threshold on *baseline + deltas* for a feature:
\$\$\sum_i z\_{is} r\_{is} + \sum\_{i,a} x\_{ia}\Delta\_{ias} \ge
T^{mix}\_s.\$\$ Because this differs from enforcing separate
conservation and recovery targets, mixed-total targets are intended to
be mutually exclusive with conservation/recovery targets for the same
feature. (Enforcement is handled by `.pa_store_targets()`).

**Overwrite behavior.** When `overwrite=TRUE`, existing targets of the
same type for the same feature(s) can be replaced. When
`overwrite=FALSE`, adding targets for features that already have targets
of that type will typically error (exact behavior is handled by
`.pa_store_targets()`).
