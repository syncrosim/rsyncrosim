# Retrieve spatial data from a SyncroSim Datasheet

**\[deprecated\]**

## Usage

``` r
datasheetSpatRaster(
  ssimObject,
  datasheet,
  column = NULL,
  scenario = NULL,
  iteration = NULL,
  timestep = NULL,
  filterColumn = NULL,
  filterValue = NULL,
  subset = NULL,
  forceElements = FALSE,
  pathOnly = FALSE
)

# S4 method for class 'character'
datasheetSpatRaster(
  ssimObject,
  datasheet,
  column = NULL,
  scenario = NULL,
  iteration = NULL,
  timestep = NULL,
  filterColumn = NULL,
  filterValue = NULL,
  subset = NULL,
  forceElements = FALSE,
  pathOnly = FALSE
)

# S4 method for class 'list'
datasheetSpatRaster(
  ssimObject,
  datasheet,
  column = NULL,
  scenario = NULL,
  iteration = NULL,
  timestep = NULL,
  filterColumn = NULL,
  filterValue = NULL,
  subset = NULL,
  forceElements = FALSE,
  pathOnly = FALSE
)

# S4 method for class 'SsimObject'
datasheetSpatRaster(
  ssimObject,
  datasheet,
  column = NULL,
  scenario = NULL,
  iteration = NULL,
  timestep = NULL,
  filterColumn = NULL,
  filterValue = NULL,
  subset = NULL,
  forceElements = FALSE,
  pathOnly = FALSE
)

# S4 method for class 'Scenario'
datasheetSpatRaster(
  ssimObject,
  datasheet,
  column = NULL,
  scenario = NULL,
  iteration = NULL,
  timestep = NULL,
  filterColumn = NULL,
  filterValue = NULL,
  subset = NULL,
  forceElements = FALSE,
  pathOnly = FALSE
)
```

## Arguments

- ssimObject:

  SsimLibrary/Project/Scenario object or list of Scenario objects. If
  SsimLibrary/Project, then `scenario` argument is required

- datasheet:

  character string. The name of the Datasheet containing the raster data

- column:

  character string. The name of the column in the datasheet containing
  the file names for raster data. If `NULL` (default) then use the first
  column that contains raster file names

- scenario:

  character string, integer, or vector of these. The Scenarios to
  include. Required if SsimObject is an SsimLibrary/Project, ignored if
  SsimObject is a list of Scenarios (optional)

- iteration:

  integer, character string, or vector of integer/character strings.
  Iteration(s) to include. If `NULL` (default) then all iterations are
  included. If no Iteration column is in the Datasheet, then ignored

- timestep:

  integer, character string, or vector of integer/character string.
  Timestep(s) to include. If `NULL` (default) then all timesteps are
  included. If no Timestep column is in the Datasheet, then ignored

- filterColumn:

  character string. The column to filter a Datasheet by. (e.g.
  "TransitionGroupID"). Note that to use the filterColumn argument, you
  must also specify a filterValue. Default is `NULL`

- filterValue:

  character string or integer (scalar or vector). The value of the
  filterColumn to filter the Datasheet by. To use the filterValue
  argument, you must also specify a filterColumn. Default is `NULL`

- subset:

  logical expression indicating Datasheet rows to return. e.g.
  expression(grepl("Ts0001", Filename, fixed=T)). See subset() for
  details (optional)

- forceElements:

  logical. If `TRUE` then returns a single raster as a RasterStack;
  otherwise returns a single raster as a RasterLayer directly. Default
  is `FALSE`

- pathOnly:

  logical. If `TRUE` then returns a list of filepaths to the raster
  files on disk. Default is `FALSE`

## Value

A SpatRaster object, or List. See terra package documentation for
details.

## Details

Please use
[`datasheet`](https://syncrosim.github.io/rsyncrosim/reference/datasheet.md)
to get the path to the raster file instead.

This function retrieves spatial columns from one or more SyncroSim
`Scenario` Datasheets.

The names of the returned SpatRaster contain metadata. For Datasheets
without Filename this is:

`paste0(<datasheet name>,".Scn",<scenario id>,".",<tif name>)`.

For Datasheets containing Filename this is:

`paste0(<datasheet name>,".Scn",<scenario id>,".It",<iteration>,".Ts",<timestep>)`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Extract specific Datasheet rasters by iteration and timestep
resultRaster <- datasheetSpatRaster(resultScenario,
                  datasheet = "helloworldSpatial_IntermediateDatasheet",
                  column = "OutputRasterFile",
                  iteration = 3,
                  timestep = 2
)

# Extract specific Datasheet SpatRasters using pattern matching
resultDatasheet <- datasheet(resultScenario,
                             name = "helloworldSpatial_IntermediateDatasheet")
outputRasterPaths <- resultDatasheet$OutputRasterFile
resultRaster <- datasheetSpatRaster(resultScenario,
                  datasheet = "helloworldSpatial_IntermediateDatasheet",
                  column = "OutputRasterFile",
                  subset = expression(grepl("ts20",
                                             outputRasterPaths,
                                             fixed = TRUE))
)

# Return the raster Datasheets as a SpatRaster list
resultRaster <- datasheetSpatRaster(resultScenario,
                 datasheet = "helloworldSpatial_IntermediateDatasheet",
                 column = "OutputRasterFile",
                 forceElements = TRUE)

# Filter for only rasters that fit specific criteria (ST-Sim example)
resultRaster <- datasheetSpatRaster(resultScenario,
                 datasheet = "stsim_OutputSpatialTransition",
                 timestep = 5,
                 iteration = 5,
                 filterColumn = "TransitionTypeID",
                 filterValue = "Fire")
} # }
```
