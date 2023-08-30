
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rsyncrosim <img src="man/figures/logo.png" align="right" width=140/>

## The R interface to SyncroSim

[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/license/mit/)
[![Downloads](https://cranlogs.r-pkg.org/badges/rsyncrosim?color=brightgreen)](https://CRAN.R-project.org/package=rsyncrosim/)
<!-- [![Codecov test coverage](https://codecov.io/gh/syncrosim/rsyncrosim/branch/dev/graph/badge.svg)](https://codecov.io/gh/syncrosim/rsyncrosim?branch=dev) -->

[![Latest
Release](https://img.shields.io/github/v/release/syncrosim/rsyncrosim?label=Latest%20Release)](https://github.com/syncrosim/rsyncrosim/releases/latest)
[![CRAN
Version](https://img.shields.io/cran/v/rsyncrosim?label=CRAN%20Version)](https://CRAN.R-project.org/package=rsyncrosim)
[![GitHub
Version](https://img.shields.io/github/r-package/v/syncrosim/rsyncrosim?label=GitHub%20Version)](https://github.com/syncrosim/rsyncrosim/blob/dev/DESCRIPTION)

## About SyncroSim

<a href="https://syncrosim.com/" target="_blank">SyncroSim</a> is a
software platform that helps you turn your *data* into *forecasts*. At
the core of SyncroSim is an engine that automatically structures your
existing data, regardless of its original format. **SyncroSim**
transforms this structured data into forecasts by running it through a
Pipeline of calculations (i.e. a suite of *models*). Finally,
**SyncroSim** provides a rich interface to interact with your data and
models, allowing you to explore and track the consequences of
alternative “what-if” forecasting Scenarios. Within this software
framework is the ability to use and create
<a href="https://docs.syncrosim.com/how_to_guides/package_overview.html" target="_blank">SyncroSim
packages</a>.

For more details consult the **SyncroSim**
<a href="https://docs.syncrosim.com/" target="_blank">online
documentation</a>.

## About `rsyncrosim`

The simplest way to run **SyncroSim** from the command-line is to use
the R programming language, in conjunction with the open-source
**rsyncrosim** R package. This R package is designed to make it simple
to script modeling workflows for **SyncroSim** in R, by providing
functions that allow users to create models from scratch, populate those
models with inputs, run the models for multiple scenarios, and access
both spatial and tabular model output. The package is designed to work
with any **SyncroSim** Package.

A key feature of the **rsyncrosim** package is its ability to work
seamlessly with the **SyncroSim** user interface. By integrating model
scripting and visualization, users can interactively explore and
validate their models in the user interface as they step through their R
code, and at the same time generate a permanent, repeatable record of
their entire modeling workflow – including both pre- and post-processing
of model inputs and outputs – in an R script.

### Installation

-   [Download](https://cran.r-project.org/package=rsyncrosim/) from CRAN

-   [Download](https://syncrosim.github.io/rsyncrosim/articles/a06_rsyncrosim_install_github.html)
    from GitHub

### Getting Started

-   Run the vignettes:

    -   [Introduction to
        `rsyncrosim`](https://syncrosim.github.io/rsyncrosim/articles/a01_rsyncrosim_vignette_basic.html)

    -   [`rsyncrosim`: introduction to
        uncertainty](https://syncrosim.github.io/rsyncrosim/articles/a02_rsyncrosim_vignette_uncertainty.html)

    -   [`rsyncrosim`: introduction to
        pipelines](https://syncrosim.github.io/rsyncrosim/articles/a03_rsyncrosim_vignette_pipelines.html)

    -   [`rsyncrosim`: introduction to spatial
        data](https://syncrosim.github.io/rsyncrosim/articles/a04_rsyncrosim_vignette_spatial.html)

    -   [Introduction to `ST-Sim` in
        `rsyncrosim`](https://syncrosim.github.io/rsyncrosim/articles/a05_rsyncrosim_stsim_vignette.html)

-   View the package
    [documentation](https://cran.r-project.org/package=rsyncrosim/rsyncrosim.pdf)
