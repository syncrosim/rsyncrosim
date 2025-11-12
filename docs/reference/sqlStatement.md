# Construct an SQLite query

Creates `SELECT`, `GROUP BY` and `WHERE` SQL statements. The resulting
list of SQL statements will be converted to an SQLite database query by
the
[`datasheet`](https://syncrosim.github.io/rsyncrosim/reference/datasheet.md)
function.

## Usage

``` r
sqlStatement(
  groupBy = NULL,
  aggregate = NULL,
  aggregateFunction = "SUM",
  where = NULL
)
```

## Arguments

- groupBy:

  character string or vector of these. Vector of variables (column
  names) to `GROUP BY` (optional)

- aggregate:

  character string of vector of these. Vector of variables (column
  names) to aggregate using `aggregateFunction` (optional)

- aggregateFunction:

  character string. An SQL aggregate function (e.g. `SUM`, `COUNT`).
  Default is `SUM`

- where:

  named list. A list of subset variables. Names are column names, and
  elements are the values to be selected from each column (optional)

## Value

Returns a list of `SELECT`, `GROUP BY` and `WHERE` SQL statements used
by the
[`datasheet`](https://syncrosim.github.io/rsyncrosim/reference/datasheet.md)
function to construct an SQLite database query.

## Details

Variables are column names of the Datasheet. See column names using
`datasheet(,empty=TRUE)` Variables not included in `groupBy`,
`aggregate` or `where` will be dropped from the table. Note that it is
not possible to construct a complete SQL query at this stage, because
the
[`datasheet`](https://syncrosim.github.io/rsyncrosim/reference/datasheet.md)
function may add ScenarioId and/or ProjectId to the query.

## Examples

``` r
# \donttest{
# Query total Amount for each combination of ScenarioId, Iteration, Timestep and StateLabelXID,
# including only Timesteps 0,1 and 2, and Iterations 3 and 4.
mySQL <- sqlStatement(
  groupBy = c("ScenarioId", "Iteration", "Timestep"),
  aggregate = c("yCum"),
  aggregateFunction = "SUM",
  where = list(Timestep = c(0, 1, 2), Iteration = c(3, 4))
)
mySQL
#> $select
#> [1] "SELECT ScenarioId,Iteration,Timestep,SUM(yCum) AS yCum"
#> 
#> $groupBy
#> [1] "GROUP BY ScenarioId,Iteration,Timestep"
#> 
#> $where
#> [1] "WHERE (Timestep IN (0,1,2)) AND (Iteration IN (3,4))"
#> 
# }
if (FALSE) { # \dontrun{
# The SQL statement can then be used in the datasheet function

# Set the file path and name of an existing SsimLibrary
myLibraryName <- file.path("MyLibrary.ssim")

# Set the SyncroSim Session, SsimLibrary, Project, and Scenario
mySession <- session()
myLibrary <- ssimLibrary(name = myLibraryName,
                         session = mySession)
myProject <- project(myLibrary, project = "Definitions")
myScenario <- scenario(myProject, scenario = "My Scenario")

# Run Scenario to generate results
resultScenario <- run(myScenario)

# Use the SQL statement when loading the Datasheet
myAggregatedDataFrame <- datasheet(resultScenario, 
                                   name = "helloworldSpatial_OutputDatasheet",
                                   sqlStatement = mySQL)
                                   
# View aggregated DataFrame
myAggregatedDataFrame
} # }
```
