## Test environments
* Windows Server 2019 (CI (GitHub actions): release 4.3.2 and devel)
* Ubuntu 20.04 (CI (GitHub actions), release 4.3.2 and devel)

# rsyncrosim 2.0.1

## Breaking changes

## Bug fixes:

* Bug fix to `run()` when running multiple scenarios at a time
* Fix bug with empty datasheets returning NA values
* Update `addPackage()` to properly update package version in a library and fix bug caused by change in SyncroSim 3 console
* Fix `delete()` function
* Fix `backup` command
* Fix bug with internal folder code having wrong "ID"" case

## New features:

* Updates to vignettes and documentation for version 2
* Add `deleteLibrary()` function specifically for deleting library objects
* Added suite of charting functions for creating SyncroSim charts from R

## Minor improvements and fixes

* Add warning for when library packages are not properly installed
* Add `forceUpdate` argument back to `ssimLibrary()`
* Change default `session()` path to "SyncroSim Studio"
* Ensure that a result scenario is output by `run()` even on run failure
* Add long path warning message

## Upstream dependencies

The SyncroSim software is an upstream dependency as rsyncrosim provides an API for it. 
Therefore, all examples, all tests, as well as vignette code, requires SyncroSim to be 
installed to run. Therefore, all tests in the submitted package should not run 
(tagged with testthat::skip_on_cran()). In addition, all vignettes are only available 
on the rsyncrosim website and are set to not be included in the package build. 
Furthermore, examples in the documentation are prevented from being checked 
using `\donttest{}`.

## R CMD check results

── R CMD check results ─────────────────────────────────── rsyncrosim 2.0.1 ────
Duration: 45s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔
