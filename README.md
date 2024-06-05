A simple demonstration of the `patter` package
================
Edward Lavender\*

<sup>\*</sup>This repository is maintained by Edward Lavender
(<edward.lavender@eawag.ch>).

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

# Introduction

This repository contains methods, written in
[R](https://www.r-project.org/) and organised as an
[RStudio](https://www.rstudio.com/)
[Project](https://r4ds.had.co.nz/workflow-projects.html), for Lavender
et al. (in prep). `patter`: particle algorithms for animal tracking in
`R` and `Julia`. In this project, we simulate animal tracking data and
apply particle filtering and smoothing algorithms to reconstruct
patterns of space use. The code forms a basic worked example of the
`patter` `R` package.

# Description

## Dependencies

The project was built in [R](https://www.r-project.org/) (version 4.3.1)
in [RStudio](https://www.rstudio.com/) and implements local dependency
management using
[`renv`](https://rstudio.github.io/renv/articles/renv.html). This
manages the installation of the
[`dv`](https://github.com/edwardlavender/dv) package (from
[GitHub](https://github.com/)), as well as other packages from the
[Comprehensive R Archive Network](https://cran.r-project.org/). The
first time the project is opened,
[`renv`](https://rstudio.github.io/renv/articles/renv.html) can be used
to regenerate the local project library, as described in `renv.lock`
(via `.Rprofile` and `renv/activate.R`).

## Directories

The project follows a standardised structure encouraged by the
[`dv`](https://github.com/edwardlavender/dv) package. The high-level
structure was generated via `dv::use_template_proj()`. The contents as
follows:

1.  **`renv/`** implements local dependency management.

2.  **`data/`** contains data:

    - `spatial/` contains spatial datasets (sourced from
      [`patter-flapper`](https://github.com/edwardlavender/patter-flapper));
    - `data.table/`, `filter/`, `smoother/`, `sim/` and `ud/` contain
      script outputs;
    - `inst/` contains [RStudio](https://www.rstudio.com/)
      [Project](https://r4ds.had.co.nz/workflow-projects.html)-management
      files generated by [`dv`](https://github.com/edwardlavender/dv):
      - `dependencies.rds` is a list of dependencies;
      - `session-info.rds` is a record of information about the `R`
        Session;
      - `tree.rds` is a record of the project directory tree (as
        generated by `dv::use_template_tree()` in `dev/01-dev.R`, see
        below); <br/>

3.  **`R/`** contains scripts for data preparation, simulation and
    analysis:

    - `preparation.R` prepares spatial dataset(s);
    - `workflow.R` simulates and analyses animal tracking datasets;
    - `figs.R` produces manuscript figures;

4.  **`dev/`** contains project-management scripts.

    - `01-dev.R` and `02-clone.R` are standard
      [`dv`](https://github.com/edwardlavender/dv) scripts:
      - `01-dev.R` records project set up and development;
      - `02-clone.R` is used to clone the project (see ‘Instructions’);

5.  **`fig/`** contains figures.

6.  **`doc/`** contains supporting documents.

Note that the `data/` (except `data/inst/`), `fig/` and `doc`
directories are not provided in the online version of this repository.

# Instructions

Follow the steps described below to clone the project and reproduce the
workflow.

1.  **Clone the project** via GitHub. Follow the instructions in
    `dev/02-clone.R` to install packages and directories:

    - **Packages.** Work through `dev/02-clone.R` to use
      [`renv`](https://rstudio.github.io/renv/articles/renv.html) to
      regenerate the local project library. Packages can also be
      manually reinstalled via `02-clone.R`. If required, `patter` is
      available [here](https://github.com/edwardlavender/patter).
    - **Directories.** Rebuild the project directory tree, via
      `dv::use_template_proj()` and `dv::use_template_tree()`.

2.  **Source (raw) data** via the links provided (or the authors).

3.  **Implement workflow** via `R` scripts.

# Code of conduct

Please note that this project is released with a [Contributor Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

------------------------------------------------------------------------