# rsyncrosim 1.4.5

## Breaking changes

* Removed the `condaFilepath` argument in the `session()` function as this was causing issues during multiprocessing; To set the conda path now, use the `condaFilepath()` replacement method on the `session` object.

## Bug fixes:

* Fixed bug that came up sometimes in `datasheet()` function when `optional` set to `TRUE` - this only came up sometimes because it had to do with the syncrosim validation type under the hood; if the validation was based on a core datasheet, then the function would fail.
* Fixed bug when trying to retrieve datasheets for multiple scenarios:
```
Error in intersect(displayName, names(lookupSheet)) : 
  object 'displayName' not found
```

## New features:

## Minor improvements and fixes
