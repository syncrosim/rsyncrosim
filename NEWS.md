# rsyncrosim 1.2.9

## Bug fixes:

* Fixes a case when `datasheet` could not handle more than one scenario when `fastQuery` was `TRUE`.
* Fixes issues with breakpoints no longer working.
* Fixes an error when trying to delete a datasheet.
* Fixes a cases when the prefix in datasheet names only worked for stsim.
* Fixes db update for old syncrosim databases.
* Fixes a case when `datasheetRaster` would not output properly when multiple iterations were requested.
* Fixes inconsistency in deletion of objects.
* Minor changes and fixes to tests and documentation.

## New features:

* Adds support for the Ignore Dependencies feature in SyncroSim. NOTE: To use this feature you must install at least version 2.2.22 of SyncroSim.