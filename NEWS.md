# rsyncrosim 2.1.13

## Breaking changes:

## Bug fixes:

- Fixed `chartOptions*`, `chartData`, and `chartErrorBar` functions throwing an error about a missing `ChartId` column
- Improved raster layer matching to ensure consistent layer selection and warnings
- Fixed missing parenthesis in `DESCRIPTION` file that was causing package builds to fail
- Fixed `README` license badge link formatting

## New features:

- Added `showFullPaths` argument to `datasheet()` to control whether columns containing external file references return filenames or fully-resolved absolute paths

## Minor improvements and fixes:

- Added `terra` as an optional dependency
- Updated package author role entries
- Added import directive for `stats::setNames`
- Improved S4 class cross-reference links and a function parameter reference

## Deprecations:
