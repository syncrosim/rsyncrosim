# Name of a SsimLibrary, Project, Scenario, Folder, or Chart

Retrieves or sets the name of a `SsimLibrary`, `Project`, `Scenario`, or
`Folder`.

## Usage

``` r
name(ssimObject)

# S4 method for class 'character'
name(ssimObject)

# S4 method for class 'SsimLibrary'
name(ssimObject)

# S4 method for class 'Scenario'
name(ssimObject)

# S4 method for class 'Project'
name(ssimObject)

# S4 method for class 'Folder'
name(ssimObject)

# S4 method for class 'Chart'
name(ssimObject)

name(ssimObject) <- value

# S4 method for class 'character'
name(ssimObject) <- value

# S4 method for class 'SsimLibrary'
name(ssimObject) <- value

# S4 method for class 'Project'
name(ssimObject) <- value

# S4 method for class 'Scenario'
name(ssimObject) <- value

# S4 method for class 'Folder'
name(ssimObject) <- value

# S4 method for class 'Chart'
name(ssimObject) <- value
```

## Arguments

- ssimObject:

  `Scenario`, `Project`, `SsimLibrary`, `Folder` or `Chart` object

- value:

  character string of the new name

## Value

A character string: the name of the SsimObject.

## Examples

``` r
# \donttest{
# Specify file path and name of new SsimLibrary
myLibraryName <- file.path(tempdir(), "testlib")

# Set up a SyncroSim Session, SsimLibrary, Project, and Scenario
mySession <- session()
myLibrary <- ssimLibrary(name = myLibraryName, 
                         session = mySession,
                         packages = "stsim")
#> Package <stsim v4.5.4> added
myProject <- project(myLibrary, project = "Definitions")
myScenario <- scenario(myProject, scenario = "My Scenario")
myFolder <- folder(myProject, folder = "New Folder")
myChart <- chart(myProject, chart = "New Chart")

# Retrieve names of the SsimObjects
name(myLibrary)
#> [1] "testlib"
name(myProject)
#> [1] "Definitions"
name(myScenario)
#> [1] "My Scenario"
name(myFolder)
#> [1] "New Folder"
name(myChart)
#> [1] "New Chart"

# Set the name of the SyncroSim Scenario
name(myScenario) <- "My Scenario Name"
# }
```
