# rsyncrosim 1.3.8

## Breaking changes

## Bug fixes:

* Fixed bug when `sourceScenario` argument in the `scenario()` function was 
set to a Scenario name.

## New features:

* New `filterValue` argument in `datasheet` and `datasheetRaster()` functions. 
The `filterValue` argument is to be used in conjunction with the `filterColumn`
argument - the `filterColumn` is the column to filter the Datasheet by, and the
`filterValue` is the value in the column to filter the Datsheet by. Note that
the Datasheet must have a primary key, or be an Output Datasheet that has a 
corresponding Input Datasheet with a primary key. See the Examples section of the
`datasheetRaster()` function for an example use case.

## Minor improvements and fixes

* Updating testing code in progress.