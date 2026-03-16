# Read-only status of a SsimLibrary, Project, Scenario, Folder or Chart

Retrieves or sets whether or not a
[`SsimLibrary`](https://syncrosim.github.io/rsyncrosim/reference/SsimLibrary-class.md),
[`Project`](https://syncrosim.github.io/rsyncrosim/reference/Project-class.md),
[`Scenario`](https://syncrosim.github.io/rsyncrosim/reference/Scenario-class.md),
[`Folder`](https://syncrosim.github.io/rsyncrosim/reference/Folder-class.md),
or
[`Chart`](https://syncrosim.github.io/rsyncrosim/reference/Chart-class.md)
is read-only.

## Usage

``` r
readOnly(ssimObject)

# S4 method for class 'character'
readOnly(ssimObject)

# S4 method for class 'SsimLibrary'
readOnly(ssimObject)

# S4 method for class 'Project'
readOnly(ssimObject)

# S4 method for class 'Scenario'
readOnly(ssimObject)

# S4 method for class 'Folder'
readOnly(ssimObject)

# S4 method for class 'Chart'
readOnly(ssimObject)

readOnly(ssimObject) <- value

# S4 method for class 'character'
readOnly(ssimObject) <- value

# S4 method for class 'SsimObject'
readOnly(ssimObject) <- value

# S4 method for class 'Folder'
readOnly(ssimObject) <- value

# S4 method for class 'Chart'
readOnly(ssimObject) <- value
```

## Arguments

- ssimObject:

  [`Scenario`](https://syncrosim.github.io/rsyncrosim/reference/Scenario-class.md),
  [`Project`](https://syncrosim.github.io/rsyncrosim/reference/Project-class.md),
  [`SsimLibrary`](https://syncrosim.github.io/rsyncrosim/reference/SsimLibrary-class.md),
  or
  [`Folder`](https://syncrosim.github.io/rsyncrosim/reference/Folder-class.md)
  object

- value:

  logical. If `TRUE` the SsimObject will be read-only. Default is
  `FALSE`

## Value

A logical: `TRUE` if the SsimObject is read-only and `FALSE` otherwise.

## Examples

``` r
# \donttest{
# Specify file path and name of new SsimLibrary
myLibraryName <- file.path(tempdir(), "testlib")

# Set up a SyncroSim Session, SsimLibrary, Project, Scenario, and Folder
mySession <- session()
myLibrary <- ssimLibrary(name = myLibraryName, session = mySession)
myProject <- project(myLibrary, project = "Definitions")
myScenario <- scenario(myProject, scenario = "My Scenario")
myFolder <- folder(myProject, "My Folder")

# Retrieve the read-only status of a SsimObject
readOnly(myLibrary)
#> [1] FALSE
readOnly(myProject)
#> [1] FALSE
readOnly(myScenario)
#> [1] FALSE
readOnly(myFolder)
#> [1] FALSE

# Set the read-only status of a SsimObject
readOnly(myScenario) <- TRUE
# }
```
