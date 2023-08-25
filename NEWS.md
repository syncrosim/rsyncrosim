# rsyncrosim 1.4.9

## Breaking changes

* Removed the `condaFilepath` argument in the `session()` function as this was causing issues during multiprocessing; To set the conda path now, use the `condaFilepath()` replacement method on the `session` object.
* Deprecated `datasheetRaster()` so will now throw a warning if used (but will still work)
* Deprecated `raster` objects as inputs to `saveDatasheet()` so will now throw a warning if used (but will still work)

## Bug fixes:

* Fixed bug in `ssimLibrary()` where `useConda` argument is set to `FALSE` as default. This wrongly updates the SyncroSim library properties when running a Library in a conda environment from the UI.
* Fixed issue where rsyncrosim "factors" erase column data if creating factor lookups fails.
* Fixed bug when loading template libraries from addon packages.
* Fixed bug that came up sometimes in `datasheet()` function when `optional` set to `TRUE` - this only came up sometimes because it had to do with the syncrosim validation type under the hood; if the validation was based on a core datasheet, then the function would fail.
* Fixed bug when trying to retrieve datasheets for multiple scenarios at a time.


## New features:

* Added the new `Folder` class that allows users to retrieve folder data for a SyncroSim Library or Project, create new folders at the Project root or nested within other folders, and move Scenarios into folders.
* Added the new `published()` function that allows a user to tag a folder for publication.
* Added the new `folderId()` function that allows a user to assign a folder ID to a Scenario (moves the scenario into the specified folder), or retrieve the folder ID for a Scenario or Folder object.
* Removed dependency on `rgdal` and `raster` functions (as these are now deprecated) and added `terra` dependency.
* Updated `saveDatasheet()` to use `terra` `spatRasters` under the hood (`raster` objects still work as well, but will throw a warning)
* Added new `datasheetSpatRaster()` function that works similarly to `datasheetRaster()`, but returns `SpatRasters` instead of `raster` objects and uses `terra` functions under the hood.


## Minor improvements and fixes

* Updated copyright