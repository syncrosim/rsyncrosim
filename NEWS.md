# rsyncrosim 2.0.0

## Breaking changes

* Updates to make compatible with SyncroSim v3.0.0
* Libraries no longer require a package on creation
* `ssimLibrary()` `package` argument has been replaced with `packages`
* The `forceUpdate`, `addons`, and `template` arguments have been removed from `ssimLibrary()`
* `addPackage()` adds a package to a library (instead of installing from server)
* `removePackage()` removes a package from a library (instead of installing from server)
* Replaced the `package()` function with the `packages()` function
* The `run()` function no longer has a `jobs` argument. The number of multiprocessing jobs is determined using the `core_Multiprocessing` datasheet
* When calling `datasheet()` with `summary=TRUE`, the scope of the `ssimObject` specified is respected
* Removed the all deprecated functions, as well as the following functions: `datasheetRaster()`, `disableAddon()`, `enableAddon()`

## Bug fixes:

* Multiple bug fixes to `Folder` class

## New features:

* Efficiency improvements for setting scenario dependencies
* `installPackage()` installs package(s) from the server or from a local file or folder and takes a `version` argument
* `removePackage()` uninstalls a package and takes a `version` argument
* New `createCondaEnv()` function for creating package conda environments using rsyncrosim
* Removed all references to deprecated `raster` package

## Minor improvements and fixes