# rsyncrosim 2.1.2

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