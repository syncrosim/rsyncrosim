# Merge dependencies for a Scenario

Retrieves or sets whether or not a
[`Scenario`](https://syncrosim.github.io/rsyncrosim/reference/Scenario-class.md)
is configured to merge dependencies at run time.

## Usage

``` r
mergeDependencies(ssimObject)

# S4 method for class 'character'
mergeDependencies(ssimObject)

# S4 method for class 'Scenario'
mergeDependencies(ssimObject)

mergeDependencies(ssimObject) <- value

# S4 method for class 'character'
mergeDependencies(ssimObject) <- value

# S4 method for class 'Scenario'
mergeDependencies(ssimObject) <- value
```

## Arguments

- ssimObject:

  [`Scenario`](https://syncrosim.github.io/rsyncrosim/reference/Scenario-class.md)
  object

- value:

  logical. If `TRUE` the Scenario will be set to merge dependencies at
  run time. Default is `FALSE`

## Value

A logical: `TRUE` if the scenario is configured to merge dependencies at
run time, and `FALSE` otherwise.

## Examples

``` r
# \donttest{
# Specify file path and name of new SsimLibrary
myLibraryName <- file.path(tempdir(),"testlib")

# Set up a SyncroSim Session, SsimLibrary, Project, and Scenario
mySession <- session()
myLibrary <- ssimLibrary(name = myLibraryName, session = mySession)
myProject <- project(myLibrary, project = "Definitions")
myScenario <- scenario(myProject, scenario = "My Scenario")

# Retrieve whether or not dependencies will be merged for a Scenario
mergeDependencies(myScenario)
#> [1] FALSE

# Set whether or not dependencies will be merged for a Scenario
mergeDependencies(myScenario) <- TRUE
# }
```
