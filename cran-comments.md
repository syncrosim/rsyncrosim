## Test environments
* Windows Server 2025 (CI (GitHub actions): release and devel)
* Ubuntu 24.04 (CI (GitHub actions), release and devel)

## New release 2.1.2

## Breaking changes

* Incremented compatible SyncroSim version to 3.1

## Bug fixes:

* Fix `filterValue` and `filterColumn` arguments in `datasheet()` and `datasheetSpatRaster()` 
* Fix issues with factor lookups not working in `datasheet()` function
* Fix bug in `installPackage()` and `uninstallPackage()` preventing package install when no packages installed yet

## New features:

* Add `software` argument to `installConda()` for installing miniforge
* Add authentication functions `signIn()`, `signOut()`, and `viewProfile()`

## Minor improvements and fixes

* Fix issue when loading a library where rsyncrosim always throws a warning that the package has not been installed properly if the package was built against SyncroSim 3.0
* Documentation updates and fixes

## Upstream dependencies

The SyncroSim software is an upstream dependency as rsyncrosim provides an API for it. 
Therefore, all examples, all tests, as well as vignette code, requires SyncroSim to be 
installed to run. Therefore, all tests in the submitted package should not run 
(tagged with testthat::skip_on_cran()). In addition, all vignettes are only 
available on the rsyncrosim website and are set to not be included in the package 
build. Furthermore, examples in the documentation are prevented from being 
checked using `\donttest{}`.

## R CMD check results


── R CMD check results ─────────────────────────────────── rsyncrosim 2.1.2 ────
Duration: 51.4s

❯ checking for future file timestamps ... NOTE
  unable to verify current time

0 errors ✔ | 0 warnings ✔ | 1 note ✖
