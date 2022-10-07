## Test environments
* Windows Server 2019 (CI (GitHub actions): release 4.1.1 and devel)
* Ubuntu 20.04 (CI (GitHub actions), release 4.1.1 and devel)

## New release 1.4.2

## Breaking changes

* Added version check to ensure the version of `rsyncrosim` being used is compatible with the version of `SyncroSim` installed - current version of `rsyncrosim` is compatible with `SyncroSim` version 2.4.4

## Bug fixes:

* Bug fixed where if the Scenario named contained square brackets, a warning was thrown
* Fixed bug in `project()` function
* `dependency()` bug fixed - function used to fail when dependency argument was set to a character
* Fixed bug in `scenario()` function when `sourceScenario` argument is set to a Scenario name
* Fixed bug in `delete()` function when trying to delete datasheets
* Fixed bug in `datasheet()` where column values could not be numeric
* Fixed cryptic warning in saveDatasheet()
* Fixed bug in `addRow()` function where column values were being auto-filled if they were a factor with one level
* Removed warnings in `addRow()` function when a tibble is supplied as the value

## New features:

* External files are no longer copied by default during multiprocessing runs (can set this using the "copyExternalInputs" argument in the `run()` function)
* Created filtering arguments in the `datasheet()` and `datasheetRaster()` functions, such that you can now specify a column to filter by (`filterColumn`) and the value to filter the column by (`filterValue`)
* Can now use addon package templates when loading an `ssimLibrary`
* New message argument in the `progressBar()` function that allows the user to add custom messages to the progress bar at runtime
* New `updateRunLog()` function that allows the user to output custom messages to the SyncroSim run log, including the ability to create multi-line run log messages and set the run status
* Added the ability to use Conda environments in `rsyncrosim` using the functions `installConda()`, `useConda()`, and `condaFilepath()`

## Minor improvements and fixes

* Created documentation on the new filtering arguments in `datasheetRaster()`
* Improved testing code

## Upstream dependencies

The SyncroSim software is an upstream dependency as rsyncrosim provides an API for it. 
Therefore, all examples, all tests, as well as vignette code, requires SyncroSim to be 
installed to run. Therefore, all tests in the submitted package should not run 
(tagged with testthat::skip_on_cran()). In addition, all vignettes are only available on the rsyncrosim website and are set to not be included in the package build. Furthermore, examples in the documentation are prevented from being checked using `\donttest{}`.

## R CMD check results

-- R CMD check results ----------------------------------- rsyncrosim 1.4.2 ----
Duration: 1m 20.8s

0 errors v | 0 warnings v | 0 notes v
