# Get, set or remove Scenario dependencies

List dependencies, set dependencies, or remove dependencies from a
SyncroSim
[`Scenario`](https://syncrosim.github.io/rsyncrosim/reference/Scenario-class.md).
Setting dependencies is a way of linking together Scenario Datafeeds,
such that a change in the Scenario that is the source dependency will
update the dependent Scenario as well.

## Usage

``` r
dependency(ssimObject)

# S4 method for class 'character'
dependency(ssimObject)

# S4 method for class 'Scenario'
dependency(ssimObject)

dependency(ssimObject) <- value

# S4 method for class 'Scenario'
dependency(ssimObject) <- value
```

## Arguments

- ssimObject:

  [`Scenario`](https://syncrosim.github.io/rsyncrosim/reference/Scenario-class.md)
  object, character string, integer, or vector of these. The Scenario
  object, name, or ID to which a dependency is to be added (or has
  already been added if `remove=TRUE`). Note that integer ids are
  slightly faster.

- value:

  [`Scenario`](https://syncrosim.github.io/rsyncrosim/reference/Scenario-class.md)
  object, character string, integer, or vector of these. The Scenario
  object, name, or ID to be used as the dependency. If an empty vector
  is provided, all dependencies are removed. If multiple elements are
  provided, elements should be ordered from highest to lowest
  precedence.

## Value

A data.frame: all dependencies for a given Scenario

## Details

If `dependency==NULL`, other arguments are ignored, and set of existing
dependencies is returned in order of precedence (from highest to lowest
precedence). Otherwise, returns list of saved or error messages for each
dependency of each scenario.

Note that pre-existing dependencies will be removed when adding new
dependencies unless those elements are included in the vector of new
dependencies.

## Examples

``` r
# \donttest{
# Specify file path and name of new SsimLibrary
myLibraryName <- file.path(tempdir(), "testlib")

# Set up a SyncroSim Session, SsimLibrary, Project, and 2 Scenarios
mySession <- session()
myLibrary <- ssimLibrary(name = myLibraryName, session = mySession)
myProject <- project(myLibrary, project = "Definitions")
myScenario <- scenario(myProject, scenario = "My Scenario")
myNewScenario <- scenario(myProject,
                          scenario = "my New Scenario")

# Set myScenario as a dependency of myNewScenario
dependency(myNewScenario) <- myScenario

# Get all dependencies info
dependency(myNewScenario)
#>   ScenarioId        Name Priority
#> 1          1 My Scenario        1

# Remove all dependencies
dependency(myNewScenario) <- c()
# }
```
