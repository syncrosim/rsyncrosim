## Test environments
* Windows Server 2025 (CI (GitHub actions): release 4.5.3 and devel)
* Ubuntu 24.04 (CI (GitHub actions), release 4.5.3 and devel)

## New release 2.1.13

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

## Upstream dependencies

The SyncroSim software is an upstream dependency as rsyncrosim provides an API for it. 
Therefore, all examples, all tests, as well as vignette code, requires SyncroSim to be 
installed to run. Therefore, all tests in the submitted package should not run 
(tagged with testthat::skip_on_cran()). In addition, all vignettes are only available on
the rsyncrosim website and are set to not be included in the package build. Furthermore,
examples in the documentation are prevented from being checked using `\donttest{}`.

## R CMD check results

── R CMD check results ────────────────────────────────── rsyncrosim 2.1.13 ────
Duration: 1m 34.2s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔