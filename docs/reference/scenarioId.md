# Retrieves scenarioId of Scenario

Retrieves the scenarioId of a `Scenario`.

## Usage

``` r
scenarioId(scenario)

# S4 method for class 'character'
scenarioId(scenario)

# S4 method for class 'Scenario'
scenarioId(scenario)
```

## Arguments

- scenario:

  `Scenario` object

## Value

Integer id of the input Scenario.

## Examples

``` r
# \donttest{
# Set the file path and name of the new SsimLibrary
myLibraryName <- file.path(tempdir(),"testlib")

# Set the SyncroSim Session, SsimLibrary, Project, and Scenario
mySession <- session()
myLibrary <- ssimLibrary(name = myLibraryName, session = mySession) 
myProject <- project(myLibrary, project = "Definitions")
myScenario <- scenario(myProject, scenario = "My Scenario")

# Get Scenario ID of Scenario
scenarioId(myScenario)
#> [1] 2
# }
```
