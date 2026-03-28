# Contributing to mosap

`mosap` is being developed as a modular framework for spatial action
planning and multi-objective optimization in R. Its development benefits
from collaboration across programming, optimization, ecology,
conservation planning, and spatial analysis. Because robust
decision-support tools require sustained development and
interdisciplinary input, contributions from different backgrounds are
very welcome. We welcome **contributions to code**, **documentation**,
**testing**, **examples**, and **methodological suggestions**.

## Improving documentation

The package documentation and website are generated automatically from
package documentation, articles, examples, and markdown sources. This
means there is no need to edit HTML manually. If you are familiar with
package documentation workflows in R, you are welcome to propose
improvements by editing .R, .Rmd, or .md source files and opening a pull
request. Documentation improvements may include:

- fixing typos or unclear wording,
- improving function documentation,
- adding examples,
- clarifying methodological details,
- improving vignettes or contribution guides.

If you are not sure how to implement a documentation change yourself,
feel free to open an [issue](https://github.com/josesalgr/mosap/issues).

## Reporting bugs

If you find a bug, please open an
[issue](https://github.com/josesalgr/mosap/issues) in the GitHub
repository.

A good bug report should include:

- a clear description of the problem,
- the function or workflow involved,
- a minimal reproducible example whenever possible,
- the expected behaviour,
- the observed behaviour,
- and session information if relevant.

Minimal reproducible examples are especially helpful because they make
it much easier to diagnose and fix the problem.

## Asking questions

If you are using `mosap` and get stuck, first check the function
documentation, examples, and available articles or package materials. If
your question is still unresolved, you can open an
[issue](https://github.com/josesalgr/mosap/issues) on GitHub. While user
support cannot always be guaranteed, questions often help improve the
documentation, reveal edge cases, or identify bugs.

## Proposing ideas and enhancements

If you have an idea for a new feature, method, or improvement in
`mosap`, please first check whether it has already been discussed in the
issue tracker.

If not, feel free to open an
[issue](https://github.com/josesalgr/mosap/issues) describing your
proposal. It is especially helpful if you explain:

- the motivation for the feature,
- the intended use case,
- how it would fit the current modular workflow,
- and whether it affects the public API, optimization model, or output
  classes.

Well-scoped proposals are usually easier to evaluate and discuss than
very broad ones.

## Contributing code

Before making contributions to the package R or C++ code, make sure
someone from the `mosap` team agrees that the change you suggest is
needed.

- Fork the package and clone onto your computer. If you haven’t done
  this before, we recommend using
  `usethis::create_from_github("josesalgr/mosap", fork = TRUE)`.

- Install all development dependences with
  [`devtools::install_dev_deps()`](https://devtools.r-lib.org/reference/install_deps.html),
  and then make sure the package passes R CMD check by running
  [`devtools::check()`](https://devtools.r-lib.org/reference/check.html).
  If R CMD check doesn’t pass cleanly, it’s a good idea to ask for help
  before continuing.

- Create a Git branch for your pull request (PR). We recommend using
  `usethis::pr_init("brief-description-of-change")`.

- Make your changes, commit to git, and then create a PR by running
  [`usethis::pr_push()`](https://usethis.r-lib.org/reference/pull-requests.html),
  and following the prompts in your browser. The title of your PR should
  briefly describe the change. The body of your PR should contain Fixes
  \#issue-number.

- For user-facing changes, add a bullet to the top of
  [NEWS.md](https://github.com/josesalgr/mosap/blob/main/NEWS.md)
  (i.e. just below the first header). Follow the style described in
  <https://style.tidyverse.org/news.html>.

## Pull requests

When opening a pull request, please make sure that:

- the proposed change is focused and clearly described,
- the package still passes
  [`devtools::check()`](https://devtools.r-lib.org/reference/check.html),
- new functionality includes tests when appropriate,
- user-facing changes include documentation updates,
- and the pull request body references the related issue when
  applicable.

If the change affects package behaviour from a user perspective, please
also add a short bullet point to
[NEWS.md](https://github.com/josesalgr/mosap/blob/main/NEWS.md) under
the most recent development section.

## Style and scope

Please try to keep contributions consistent with the current design of
`mosap`, especially its modular structure:

- `inputData()` builds the problem object,
- `add_*()` functions enrich the problem,
- `set_*()` functions configure methods or solver settings,
- [`solve()`](https://josesalgr.github.io/mosap/reference/solve.md)
  executes the optimization workflow,
- `get_*()` and plotting functions expose results.

Changes that preserve and strengthen this modular structure are usually
preferred over monolithic shortcuts or tightly coupled additions.
