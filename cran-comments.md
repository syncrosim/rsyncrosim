## Test environments
* Windows Server 2019 (CI (GitHub actions): release 4.3.2 and devel)
* Ubuntu 20.04 (CI (GitHub actions), release 4.3.2 and devel)

## New release 2.1.9

## Breaking changes:

* Removed the following arguments from saveDatasheet():
** fileData
** forceElements
** breakpoint
** import
** path

## Bug fixes:

* Bug fix to prevent overwriting of default invisible values in project-scope datasheets
* Fix error when dataframe columns of type double contain both NAs and values and you try to use saveDatasheet()
* Fix bug in `saveDatasheet()` preventing saving of core datasheets

## New features:

* Added restore function to restore a SyncroSim library backup to a .ssim file
* Added compact function to clean up the underlying database of a SyncroSim library

## Minor improvements and fixes:

* Refactored saveDatasheet
* Error handling for when `data` value is not in R data.frame format in `saveDatasheet()`
* Add error handling when version command in session() fails

## Upstream dependencies

The SyncroSim software is an upstream dependency as rsyncrosim provides an API for it. 
Therefore, all examples, all tests, as well as vignette code, requires SyncroSim to be 
installed to run. Therefore, all tests in the submitted package should not run 
(tagged with testthat::skip_on_cran()). In addition, all vignettes are only available on the rsyncrosim website and are set to not be included in the package build. Furthermore, examples in the documentation are prevented from being checked using `\donttest{}`.

## R CMD check results

── R CMD check results ────────────────────────────────────────────────────────────────────────────────────────── rsyncrosim 2.1.9 ────
Duration: 1m 23.2s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔
