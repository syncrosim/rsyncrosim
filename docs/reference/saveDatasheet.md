# Save datasheet

Saves a datasheet to a
[`SsimLibrary`](https://syncrosim.github.io/rsyncrosim/reference/SsimLibrary-class.md),
[`Project`](https://syncrosim.github.io/rsyncrosim/reference/Project-class.md),
or
[`Scenario`](https://syncrosim.github.io/rsyncrosim/reference/Scenario-class.md).

## Usage

``` r
saveDatasheet(ssimObject, data, name = NULL, append = NULL, force = FALSE)

# S4 method for class 'character'
saveDatasheet(ssimObject, data, name = NULL, append = NULL, force = FALSE)

# S4 method for class 'SsimObject'
saveDatasheet(ssimObject, data, name = NULL, append = NULL, force = FALSE)
```

## Arguments

- ssimObject:

  [`SsimLibrary`](https://syncrosim.github.io/rsyncrosim/reference/SsimLibrary-class.md),
  [`Project`](https://syncrosim.github.io/rsyncrosim/reference/Project-class.md),
  or
  [`Scenario`](https://syncrosim.github.io/rsyncrosim/reference/Scenario-class.md)
  object

- data:

  data.frame. The datasheet to load

- name:

  character. The name of the datasheet to be saved

- append:

  logical. If `TRUE`, the incoming data will be appended to the
  datasheet if possible. Default is `TRUE` for Project/SsimLibrary-scope
  datasheets, and `FALSE` for Scenario-scope Datasheets. See 'details'
  for more information about this argument

- force:

  logical. If datasheet scope is Project/SsimLibrary, and
  `append=FALSE`, datasheet will be deleted before loading the new data.
  This can also delete other definitions and results, so if
  `force=FALSE` (default) user will be prompted for approval

## Value

Invisibly returns a vector or list of logical values for each input:
`TRUE` upon success (i.e.successful save) and `FALSE` upon failure.

## Details

About the 'append' argument:

- A Datasheet is a VALIDATION SOURCE if its data can be used to validate
  column values in a different Datasheet.

- The `append` argument will be ignored if the Datasheet is a validation
  source and has a Project scope. In this case the data will be MERGED.

## Examples

``` r
if (FALSE) { # \dontrun{
# Specify file path and name of new SsimLibrary
myLibraryName <- file.path(tempdir(), "testlib")

# Set the SyncroSim Session, SsimLibrary, Project, and Scenario
mySession <- session()
myLibrary <- ssimLibrary(name = myLibraryName,
                         session = mySession,
                         packages = "helloworldSpatial")
myProject <- project(myLibrary, project = "Definitions")
myScenario <- scenario(myProject, scenario = "My Scenario")

# Get all Datasheet info
myDatasheets <- datasheet(myScenario)

# Get a specific Datasheet
myDatasheet <- datasheet(myScenario, name = "helloworldSpatial_RunControl")

# Modify Datasheet
myDatasheet$MaximumTimestep <- 10

# Save Datasheet
saveDatasheet(ssimObject = myScenario, 
              data = myDatasheet, 
              name = "helloworldSpatial_RunControl")
} # }
```
