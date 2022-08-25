# rsyncrosim 1.4.0

## Breaking changes

* Upgrading minor version to be compatible with SyncroSim v2.4

## Bug fixes:

* Fixed version check code in `session()`

## New features:

* Run log function can now set the run status

## Minor improvements and fixes

* Removed warnings in `addRow()` function when a tibble is supplied as the value
* Fixed bug in `addRow()` function where column values were being auto-filled if they were a factor with one level
* Fixed cryptic warning in `saveDatasheet()`