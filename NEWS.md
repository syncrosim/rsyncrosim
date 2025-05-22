# rsyncrosim 2.1.3

## Breaking changes:

* Removed the following arguments from `saveDatasheet()`:
  * `fileData`
  * `forceElements`
  * `breakpoint`
  * `import`
  * `path`

## Bug fixes:

* Fix error when dataframe columns of type double contain both NAs and values and you try to use `saveDatasheet()`

## New features:

## Minor improvements and fixes:

* Refactored `saveDatasheet`

