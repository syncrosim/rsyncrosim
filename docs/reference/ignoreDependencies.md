# Ignore dependencies for a Scenario

Retrieves or sets the Datafeeds to ignore for a
[`Scenario`](https://syncrosim.github.io/rsyncrosim/reference/Scenario-class.md).

## Usage

``` r
ignoreDependencies(ssimObject)

# S4 method for class 'character'
ignoreDependencies(ssimObject)

# S4 method for class 'Scenario'
ignoreDependencies(ssimObject)

ignoreDependencies(ssimObject) <- value

# S4 method for class 'character'
ignoreDependencies(ssimObject) <- value

# S4 method for class 'Scenario'
ignoreDependencies(ssimObject) <- value
```

## Arguments

- ssimObject:

  [`Scenario`](https://syncrosim.github.io/rsyncrosim/reference/Scenario-class.md)
  object

- value:

  character string of Datafeed names to be ignored, separated by commas
  (optional)

## Value

A character string: Scenario Datafeeds that will be ignored.

## Examples

``` r
# \donttest{
# Specify file path and name of new SsimLibrary
myLibraryName <- file.path(tempdir(), "testlib")

# Set up a SyncroSim Session, SsimLibrary, Project, and Scenario
mySession <- session()
myLibrary <- ssimLibrary(name = myLibraryName, session = mySession)
myProject <- project(myLibrary, project = "Definitions")
myScenario <- scenario(myProject, scenario = "My Scenario")

# List the Datafeeds to ignore
ignoreDependencies(myScenario)
#> NULL

# Set Scenario Datafeeds to ignore
ignoreDependencies(myScenario) <- "stsim_RunControl,stsim_TransitionTarget"
# }
```
