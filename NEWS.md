# rsyncrosim 2.1.9

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
