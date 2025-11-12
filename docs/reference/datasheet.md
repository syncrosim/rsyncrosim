# Retrieve a SyncroSim Datasheet

This function retrieves a SyncroSim Datasheet, either by calling the
SyncroSim console, or by directly querying the `SsimLibrary` database.

## Usage

``` r
datasheet(
  ssimObject,
  name = NULL,
  project = NULL,
  scenario = NULL,
  summary = NULL,
  optional = FALSE,
  empty = FALSE,
  filterColumn = NULL,
  filterValue = NULL,
  lookupsAsFactors = TRUE,
  sqlStatement = list(select = "SELECT *", groupBy = ""),
  includeKey = FALSE,
  forceElements = FALSE,
  fastQuery = FALSE,
  returnScenarioInfo = FALSE,
  returnInvisible = FALSE,
  rawValues = FALSE,
  verbose = TRUE
)

# S4 method for class 'list'
datasheet(
  ssimObject,
  name = NULL,
  project = NULL,
  scenario = NULL,
  summary = NULL,
  optional = FALSE,
  empty = FALSE,
  filterColumn = NULL,
  filterValue = NULL,
  lookupsAsFactors = TRUE,
  sqlStatement = list(select = "SELECT *", groupBy = ""),
  includeKey = FALSE,
  forceElements = FALSE,
  fastQuery = FALSE,
  returnScenarioInfo = FALSE,
  returnInvisible = FALSE,
  rawValues = FALSE,
  verbose = TRUE
)

# S4 method for class 'character'
datasheet(
  ssimObject,
  name,
  project,
  scenario,
  summary,
  optional,
  empty,
  filterColumn,
  filterValue,
  lookupsAsFactors,
  sqlStatement,
  includeKey,
  fastQuery,
  returnScenarioInfo,
  returnInvisible,
  rawValues,
  verbose
)

# S4 method for class 'SsimObject'
datasheet(
  ssimObject,
  name = NULL,
  project = NULL,
  scenario = NULL,
  summary = NULL,
  optional = FALSE,
  empty = FALSE,
  filterColumn = NULL,
  filterValue = NULL,
  lookupsAsFactors = TRUE,
  sqlStatement = list(select = "SELECT *", groupBy = ""),
  includeKey = FALSE,
  forceElements = FALSE,
  fastQuery = FALSE,
  returnScenarioInfo = FALSE,
  returnInvisible = FALSE,
  rawValues = FALSE,
  verbose = TRUE
)
```

## Arguments

- ssimObject:

  `SsimLibrary`, `Project`, or `Scenario` object or list of objects.
  Note that all objects in a list must be of the same type, and belong
  to the same SsimLibrary

- name:

  character or character vector. Sheet name(s). If `NULL` (default), all
  datasheets in the ssimObject will be returned. Note that setting
  `summary=FALSE` and `name=NULL` pulls all Datasheets, which is time
  consuming and not generally recommended

- project:

  numeric or numeric vector. One or more `Project` ids

- scenario:

  numeric or numeric vector. One or more `Scenario` ids

- summary:

  logical or character. If `TRUE` (default) returns a data.frame of
  sheet names and other info including built-in core SyncroSim
  Datasheets. If `FALSE` returns data.frame or list of data.frames.

- optional:

  logical. If `summary=TRUE` and `optional=TRUE` returns only `scope`,
  `name` and `displayName`. If `summary=FALSE` and `optional=TRUE`
  returns all of the Datasheet's columns, including the optional
  columns. If `summary=TRUE`, `optional=FALSE` (default), returns only
  those columns that are mandatory and contain data (if `empty=FALSE`).
  Ignored if `summary=FALSE`, `empty=FALSE` and `lookupsAsFactors=FALSE`

- empty:

  logical. If `TRUE` returns empty data.frames for each Datasheet.
  Ignored if `summary=TRUE` Default is `FALSE`

- filterColumn:

  character string. The column to filter a Datasheet by. (e.g.
  "TransitionGroupId"). Note that to use the filterColumn argument, you
  must also specify the filterValue argument. Default is `NULL`

- filterValue:

  character string or integer. The value to filter the filterColumn by.
  To use the filterValue argument, you must also specify the
  filterColumn argument. Default is `NULL`

- lookupsAsFactors:

  logical. If `TRUE` (default) dependencies returned as factors with
  allowed values (levels). Set `FALSE` to speed calculations. Ignored if
  `summary=TRUE`

- sqlStatement:

  list returned by
  [`sqlStatement`](https://syncrosim.github.io/rsyncrosim/reference/sqlStatement.md).
  `SELECT` and `GROUP BY` SQL statements passed to SQLite database.
  Ignored if `summary=TRUE` (optional)

- includeKey:

  logical. If `TRUE` include primary key in table. Default is `FALSE`

- forceElements:

  logical. If `FALSE` (default) and name has a single element returns a
  data.frame; otherwise returns a list of data.frames. Ignored if
  `summary=TRUE`

- fastQuery:

  logical. If `TRUE`, the request is optimized for performance. Ignored
  if combined with summary, empty, or
  [`sqlStatement`](https://syncrosim.github.io/rsyncrosim/reference/sqlStatement.md)
  flags. Default is `FALSE`

- returnScenarioInfo:

  logical. If `TRUE`, returns the Scenario Id, Scenario Name, Parent Id,
  and Parent Name columns with the Scenario-scoped Datasheet. Does
  nothing if the Datasheet exists at the Library or Project level.
  Default is `FALSE`

- returnInvisible:

  logical. If `TRUE`, returns columns that are invisible in the User
  Interface (i.e., are only used and populated internally by SyncroSim
  or the SyncroSim Package). Default is `FALSE`

- rawValues:

  logical. If `TRUE`, returns the raw ID values rather than
  automatically translating the values to strings. Default is `FALSE`.

- verbose:

  logical. If set to `FALSE`, will not print notes about datasheet
  validation. Default is `TRUE`.

## Value

If `summary=TRUE` returns a data.frame of Datasheet names and other
information, otherwise returns a data.frame or list of these.

## Details

If `summary=TRUE` or `summary=NULL` and `name=NULL` a data.frame
describing the Datasheets is returned. If `optional=TRUE`, columns
include: `scope`, `packages`, `name`, `displayName`, `isSingle`, `data`.
data only displayed for a SyncroSim `Scenario`. `dataInherited` and
`dataSource` columns added if a Scenario has dependencies. If
`optional=FALSE`, columns include: `scope`, `name`, `displayName`. All
other arguments are ignored.

Otherwise, for each element in name a Datasheet is returned as follows:

- If `lookupsAsFactors=TRUE` (default): Each column is given the correct
  data type, and dependencies returned as factors with allowed values
  (levels). A warning is issued if the lookup has not yet been set.

- If `empty=TRUE`: Each column is given the correct data type. Fast (1
  less console command).

- If `empty=FALSE` and `lookupsAsFactors=FALSE`: Column types are not
  checked, and the optional argument is ignored. Fast (1 less console
  command).

- If SsimObject is a list of `Scenario` or `Project` objects (output
  from [`run`](https://syncrosim.github.io/rsyncrosim/reference/run.md),
  `Scenario` or `Project`): Adds ScenarioId/ProjectId column if
  appropriate.

- If Scenario/Project is a vector: Adds ScenarioId/ProjectId column as
  necessary.

- If requested Datasheet has Scenario scope and contains info from more
  than one Scenario: ScenarioId/ScenarioName/ScenarioParent columns
  identify the Scenario by `name`, `id`, and `parent` (if a result
  Scenario).

- If requested Datasheet has Project scope and contains info from more
  than one Project: ProjectId/ProjectName columns identify the Project
  by `name` and `id`

## Examples

``` r
if (FALSE) { # \dontrun{
# Install helloworldSpatial package from package server
installPackage("helloworldSpatial")

# Set the file path and name of the new SsimLibrary
myLibraryName <- file.path(tempdir(),"testlib_datasheet")

# Set the SyncroSim Session
mySession <- session()

# Create a new SsimLibrary with the example template from helloworldSpatial
myLibrary <- ssimLibrary(name = myLibraryName,
                         session = mySession, 
                         packages = "helloworldSpatial")
                         
# Set the Project and Scenario
myProject <- project(myLibrary, project = "Definitions")
myScenario <- scenario(myProject, scenario = "My Scenario")

# Get all Datasheet info for the Scenario
myDatasheets <- datasheet(myScenario)


# Return a list of data.frames (1 for each Datasheet)
myDatasheetList <- datasheet(myScenario, summary = FALSE)

# Get a specific Datasheet
myDatasheet <- datasheet(myScenario, name = "helloworldSpatial_RunControl")

# Include primary key when retrieving a Datasheet
myDatasheet <- datasheet(myScenario, name = "helloworldSpatial_RunControl", 
                         includeKey = TRUE)

# Return all columns, including optional ones
myDatasheet <- datasheet(myScenario, name = "helloworldSpatial_RunControl", 
                         summary = TRUE, optional = TRUE)

# Return Datasheet as an element
myDatasheet <- datasheet(myScenario, name = "helloworldSpatial_RunControl", 
                         forceElements = TRUE)
myDatasheet$helloworldSpatial_RunControl

# Get a Datasheet without pre-specified values
myDatasheetEmpty <- datasheet(myScenario, 
                              name = "helloworldSpatial_RunControl", 
                              empty = TRUE)

# If Datasheet is empty, do not return dependencies as factors
myDatasheetEmpty <- datasheet(myScenario, 
                              name = "helloworldSpatial_RunControl", 
                              empty = TRUE,
                              lookupsAsFactors = FALSE)
                              
# Optimize query
myDatasheet <- datasheet(myScenario, name = "helloworldSpatial_RunControl", 
                         fastQuery = TRUE)

# Get specific SsimLibrary core Datasheet
myDatasheet <- datasheet(myLibrary, name = "core_Backup")

# Use an SQL statement to query a Datasheet
mySQL <- sqlStatement(
  groupBy = c("ScenarioId"),
  aggregate = c("MinimumTimestep"),
  where = list(MinimumTimestep = c(1))
)
myAggregatedDatasheet <- datasheet(myScenario, 
                                   name = "helloworldSpatial_RunControl",
                                   sqlStatement = mySQL)
} # }
```
