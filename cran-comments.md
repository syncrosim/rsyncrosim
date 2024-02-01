## Test environments
* Windows Server 2019 (CI (GitHub actions): release 4.3.2 and devel)
* Ubuntu 20.04 (CI (GitHub actions), release 4.3.2 and devel)

## New release 1.5.0

## Breaking changes

* Added `returnScenarioInfo` argument to `datasheet()` function that adds the `Scenario ID`, `Scenario Name`, `Project ID`, `Parent ID`, and `Parent Name` columns to the output dataframe. By default, this argument is `FALSE`. The behaviour in v1.4.14 was to always return these columns.
* Added `returnInvisible` argument to `datasheet()` function that, when set to `TRUE`, returns columns that are invisible in the SyncroSim User Interface. These columns are only used internally by SyncroSim and its Packages, and should normally not be edited or visible by the average SyncroSim User. By default, this argument is `FALSE`. The behaviour in v1.4.14 was to always return these columns.
* Removed the `published()` function

## Bug fixes:

* Fixed bug in `folder()` function - issue retrieving existing folder when dependencies added to the library structure

## Minor improvements and fixes

* Updated copyright

## Upstream dependencies

The SyncroSim software is an upstream dependency as rsyncrosim provides an API for it. 
Therefore, all examples, all tests, as well as vignette code, requires SyncroSim to be 
installed to run. Therefore, all tests in the submitted package should not run 
(tagged with testthat::skip_on_cran()). In addition, all vignettes are only available on the rsyncrosim website and are set to not be included in the package build. Furthermore, examples in the documentation are prevented from being checked using `\donttest{}`.

## R CMD check results

── R CMD check results ─────────────────────────────────── rsyncrosim 1.5.0 ────
Duration: 58.2s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔
