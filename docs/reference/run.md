# Run scenarios

Run one or more SyncroSim
[`Scenario`](https://syncrosim.github.io/rsyncrosim/reference/Scenario-class.md)(s).

## Usage

``` r
run(ssimObject, scenario = NULL, summary = FALSE, transformerName = NULL)

# S4 method for class 'character'
run(ssimObject, scenario = NULL, summary = FALSE, transformerName = NULL)

# S4 method for class 'list'
run(ssimObject, scenario = NULL, summary = FALSE, transformerName = NULL)

# S4 method for class 'SsimObject'
run(ssimObject, scenario = NULL, summary = FALSE, transformerName = NULL)
```

## Arguments

- ssimObject:

  [`SsimLibrary`](https://syncrosim.github.io/rsyncrosim/reference/SsimLibrary-class.md),
  [`Project`](https://syncrosim.github.io/rsyncrosim/reference/Project-class.md),
  or
  [`Scenario`](https://syncrosim.github.io/rsyncrosim/reference/Scenario-class.md)
  object, or a list of Scenarios, or character (i.e. path to a
  SsimLibrary on disk)

- scenario:

  character, integer, or vector of these. Scenario names or ids. If
  `NULL` (default), then runs all Scenarios associated with the
  SsimObject. Note that integer ids are slightly faster

- summary:

  logical. If `FALSE` (default) result Scenario objects are returned. If
  `TRUE` (faster) result Scenario ids are returned

- transformerName:

  character. The name of the transformer to run (optional)

## Value

If `summary = FALSE`, returns a result Scenario object or a named list
of result Scenarios. The name is the parent Scenario for each result. If
`summary = TRUE`, returns summary info for result Scenarios.

## Details

Note that breakpoints are ignored unless the SsimObject is a single
Scenario.

## Examples

``` r
if (FALSE) { # \dontrun{
# Set the file path and name of the new SsimLibrary
myLibraryName <- "testlib"

# Set the SyncroSim Session, SsimLibrary, Project, and Scenario
myLibrary <- ssimLibrary(name = myLibraryName,
                         packages = "helloworldSpatial")
myProject <- project(myLibrary, project = "Definitions")
myScenario <- scenario(myProject, scenario = "My Scenario")
myScenario2 <- scenario(myProject, scenario = "My Scenario 2")

# Run with default parameters
resultScenario <- run(myScenario)

# Only return summary information
resultScenarioSummary <- run(myScenario, summary = TRUE)

# Run 2 scenarios at once
resultScenarios <- run(c(myScenario, myScenario2))
} # }
```
