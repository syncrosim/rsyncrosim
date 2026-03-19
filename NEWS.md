# rsyncrosim 2.1.13

## Breaking changes:

## Bug fixes:

* Improved column matching when populating datasheets to treat column names case-insensitively, reducing mismatches
* When a case-insensitive match is found, sheet column names are aligned to the canonical names for consistent output
* Preserved previous behaviors for ID assignment and full-selection imports to maintain compatibility
* Restrict renaming of "ID"→"Id" to primary key and foreign key columns only, avoiding unintended renames in other column
* Bug fix to allow overwriting datasheet with empty dataframe
* Updates to datasheet and datasheetSpatRaster to fail if filterColumn doesn't
  exist in datasheet
* Bug fix to prevent duplicate records when specifying multiple scenarios within
  the same datasheet function call


## Minor improvements and fixes:
* Updated package dependencies
* Fixed links for classes in documentation (scenario, project, library, folder)
* datasheet filterValue argument now accepts vectors to allow filtering on multiple values at once


## Deprecations:

* datasheetSpatRaster() is now deprecated; use datasheet() instead to obtain raster file paths
