
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rsyncrosim <img src="inst/images/sticker.png" align="right" width=140/>

## The R interface to SyncroSim

[![License: GPL
v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](http://www.gnu.org/licenses/gpl-3.0)
![R-CMD-check](https://github.com/syncrosim/rsyncrosim/workflows/R-CMD-check/badge.svg)

## About SyncroSim

  - **Automatically structure your data:** SyncroSim ties together your
    existing data from disparate formats, such as Excel, CSV, and
    GeoTIFF. No database is required – SyncroSim automatically
    structures your data files for you.

  - **Use your data to make predictions:** SyncroSim allows you to chain
    together “models” – including existing off-the-shelf programs and
    scripts written in languages such as R, Python and C\# – to
    transform your data into predictions.

  - **Engage decision makers:** No longer are forecasts delivered by
    analysts as static reports. Through its unique, easy-to-use
    interface, SyncroSim allows non-technical users to define, run and
    track their own “what-if” scenarios using the original data and
    models.

  - **Go big:** SyncroSim is specifically designed to handle big data,
    including support for cloud computing, multiprocessing, and large
    rasters.

## About `rsyncrosim`

The package `rsyncrosim` is an interface for SyncroSim, a generalized framework for managing scenario-based datasets. Simulation models can be added to SyncroSim in order to transform these datasets, taking advantage of general features such as defining scenarios of model inputs, running Monte Carlo simulations, and summarizing model outputs.

### Installation

*Windows:*

  - [Download](https://github.com/syncrosim/rsyncrosim/releases/) the
    latest latest release of the Windows rsyncrosim package as a zip
    file (note that the package is not yet available from CRAN)

  - Install the package zip file in R. For example in RStudio, open the
    Install Packages window, select Package Archive File (for Install
    from) and rsyncrosim\_X.X.X.zip (for Package archive), where X.X.X
    is the version of the package.

*Linux:*

Download and install the latest release of the rsyncrosim package.

### Getting Started

  - Run the [demonstration
    script](https://github.com/syncrosim/rsyncrosim/blob/dev/demo/rsyncrosim-demo.R)
    
  - View the package [documentation](https://github.com/syncrosim/rsyncrosim/blob/dev/rsyncrosim_1.2.pdf)
