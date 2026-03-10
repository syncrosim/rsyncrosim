# Retrieves projectId of SyncroSim Project, Scenario, Folder, or Chart

Retrieves the projectId of a SyncroSim
[`Project`](https://syncrosim.github.io/rsyncrosim/reference/Project-class.md),
[`Scenario`](https://syncrosim.github.io/rsyncrosim/reference/Scenario-class.md),
[`Folder`](https://syncrosim.github.io/rsyncrosim/reference/Folder-class.md)
or
[`Chart`](https://syncrosim.github.io/rsyncrosim/reference/Chart-class.md).

## Usage

``` r
projectId(ssimObject)

# S4 method for class 'character'
projectId(ssimObject)

# S4 method for class 'Project'
projectId(ssimObject)

# S4 method for class 'Scenario'
projectId(ssimObject)

# S4 method for class 'Folder'
projectId(ssimObject)

# S4 method for class 'Chart'
projectId(ssimObject)
```

## Arguments

- ssimObject:

  [`Scenario`](https://syncrosim.github.io/rsyncrosim/reference/Scenario-class.md),
  [`Project`](https://syncrosim.github.io/rsyncrosim/reference/Project-class.md),
  [`Folder`](https://syncrosim.github.io/rsyncrosim/reference/Folder-class.md),
  or
  [`Chart`](https://syncrosim.github.io/rsyncrosim/reference/Chart-class.md)
  object

## Value

An integer: project id.

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

# Get Project ID for SyncroSim Project and Scenario
projectId(myProject)
#> [1] 1
projectId(myScenario)
#> [1] 1
# }
```
