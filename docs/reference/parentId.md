# Retrieves the parent Scenario id or parent Folder id

Retrieves the id of the parent of a SyncroSim results Scenario or a
SyncroSim Folder.

## Usage

``` r
parentId(child)

# S4 method for class 'character'
parentId(child)

# S4 method for class 'Scenario'
parentId(child)

# S4 method for class 'Folder'
parentId(child)
```

## Arguments

- child:

  [`Scenario`](https://syncrosim.github.io/rsyncrosim/reference/Scenario-class.md)
  or
  [`Folder`](https://syncrosim.github.io/rsyncrosim/reference/Folder-class.md)
  object

## Value

An integer id of the parent Scenario if input is a Scenario, or an
integer id of the parent Folder if input is a Folder. If the input
Scenario or Folder does not have a parent, the function returns `NA`

## Examples

``` r
if (FALSE) { # \dontrun{
# Set the file path and name of an existing SsimLibrary
myLibraryName <- "MyLibrary.ssim"

# Set the SyncroSim Session, SsimLibrary, Project, and Scenario
mySession <- session()
myLibrary <- ssimLibrary(name = myLibraryName,
                         session = mySession)
myProject <- project(myLibrary, project = "Definitions")
myScenario <- scenario(myProject, scenario = "My Scenario")

# Run Scenario to generate results
resultScenario <- run(myScenario)

# Find the parent ID of the Scenario
parentId(resultScenario)
} # }
```
