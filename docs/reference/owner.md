# Owner of a SsimLibrary, Project, or Scenario

Retrieves or sets the owner of a
[`SsimLibrary`](https://syncrosim.github.io/rsyncrosim/reference/SsimLibrary-class.md),
[`Project`](https://syncrosim.github.io/rsyncrosim/reference/Project-class.md),
or
[`Scenario`](https://syncrosim.github.io/rsyncrosim/reference/Scenario-class.md).

## Usage

``` r
owner(ssimObject)

owner(ssimObject) <- value

# S4 method for class 'character'
owner(ssimObject)

# S4 method for class 'SsimLibrary'
owner(ssimObject)

# S4 method for class 'Project'
owner(ssimObject)

# S4 method for class 'Scenario'
owner(ssimObject)

# S4 method for class 'character'
owner(ssimObject) <- value

# S4 method for class 'SsimObject'
owner(ssimObject) <- value
```

## Arguments

- ssimObject:

  [`Session`](https://syncrosim.github.io/rsyncrosim/reference/Session-class.md),
  [`Project`](https://syncrosim.github.io/rsyncrosim/reference/Project-class.md),
  or
  [`SsimLibrary`](https://syncrosim.github.io/rsyncrosim/reference/SsimLibrary-class.md)
  object

- value:

  character string of the new owner

## Value

A character string: the owner of the SsimObject.

## Examples

``` r
if (FALSE) { # \dontrun{
# Specify file path and name of new SsimLibrary
myLibraryName <- file.path(tempdir(), "testlib")

# Set up a SyncroSim Session, SsimLibrary, Project, and Scenario
mySession <- session()
myLibrary <- ssimLibrary(name = myLibraryName, session = mySession)
myProject <- project(myLibrary, project = "Definitions")
myScenario <- scenario(myProject, scenario = "My Scenario")

# Retrieve the owner of an SsimObject
owner(myLibrary)
owner(myProject)
owner(myScenario)

# Set the owner of a SyncroSim Scenario
owner(myScenario) <- "Apex RMS"
} # }
```
