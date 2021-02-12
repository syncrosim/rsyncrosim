## Test environments
* Windows 10 CI (CI (GitHub actions): release 3.6.3 and devel)
* Ubuntu 18.04 (CI (GitHub actions), release 3.6.3 and devel)

## New release 1.2.9

### Bug fixes:

* Fixes a case when `datasheet` could not handle more than one scenario when `fastQuery` was `TRUE`.
* Fixes issues with breakpoints no longer working.
* Fixes an error when trying to delete a datasheet.
* Fixes a cases when the prefix in datasheet names only worked for stsim.
* Fixes db update for old syncrosim databases.
* Fixes a case when `datasheetRaster` would not output properly when multiple iterations were requested.
* Fixes inconsistency in deletion of objects.
* Minor changes and fixes to tests and documentation.

### New features:

* Adds support for the Ignore Dependencies feature in SyncroSim. NOTE: To use this feature you must install at least version 2.2.22 of SyncroSim.

## Upstream dependencies

The Syncrosim software is an upstream dependency as rsyncrosim provides an API for it. 
Therefore, all examples, all tests, as well as vignette code, requires Syncrosim to be 
installed to run. Therefore, all tests in the submitted package should not run 
(tagged with testthat::skip_on_cran()). In addition, all code from vignettes is 
set to not be evaluated wehn NOT_CRAN is FALSE is the environment.

## R CMD check results

-- R CMD check results ----------------------------------- rsyncrosim 1.2.9 ----
Duration: 1m 18s

0 errors v | 0 warnings v | 0 notes v
