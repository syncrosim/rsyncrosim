# Retrieves run log of result Scenario

Retrieves the run log of a result Scenario.

## Usage

``` r
runLog(scenario)

# S4 method for class 'character'
runLog(scenario)

# S4 method for class 'Scenario'
runLog(scenario)
```

## Arguments

- scenario:

  `Scenario` object.

## Value

A character string: the run log for a result scenario.

## Examples

``` r
if (FALSE) { # \dontrun{
# Set the file path and name of an existing SsimLibrary
myLibraryName <- file.path("MyLibrary.ssim")

# Set the SyncroSim Session, SsimLibrary, Project, and Scenario
mySession <- session()
myLibrary <- ssimLibrary(name = myLibraryName,
                         session = mySession)
myProject <- project(myLibrary, project = "Definitions")
myScenario <- scenario(myProject, scenario = "My Scenario")

# Run Scenario
resultScenario <- run(myScenario)

# Retrieve the run log of the result Scenario
runLog(resultScenario)
} # }
```
