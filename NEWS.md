# rsyncrosim 1.5.0

## Breaking changes

* Added `returnScenarioInfo` argument to `datasheet()` function that adds the `Scenario ID`, `Scenario Name`, `Project ID`, `Parent ID`, and `Parent Name` columns to the output dataframe. By default, this argument is `FALSE`. The behaviour in v1.4.14 was to always return these columns.
* Added `returnInvisible` argument to `datasheet()` function that, when set to `TRUE`, returns columns that are invisible in the SyncroSim User Interface. These columns are only used internally by SyncroSim and its Packages, and should normally not be edited or visible by the average SyncroSim User. By default, this argument is `FALSE`. The behaviour in v1.4.14 was to always return these columns.

## Bug fixes:

* Fixed bug in `folder()` function - issue retrieving existing folder when dependencies added to the library structure

## New features:


## Minor improvements and fixes