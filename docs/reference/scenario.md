# Create or open Scenario(s)

Create or open one or more
[`Scenario`](https://syncrosim.github.io/rsyncrosim/reference/Scenario-class.md)s
from a
[`SsimLibrary`](https://syncrosim.github.io/rsyncrosim/reference/SsimLibrary-class.md).

## Usage

``` r
scenario(
  ssimObject = NULL,
  scenario = NULL,
  sourceScenario = NULL,
  folder = NULL,
  summary = NULL,
  results = FALSE,
  forceElements = FALSE,
  overwrite = FALSE
)
```

## Arguments

- ssimObject:

  [`SsimLibrary`](https://syncrosim.github.io/rsyncrosim/reference/SsimLibrary-class.md)
  or
  [`Project`](https://syncrosim.github.io/rsyncrosim/reference/Project-class.md)
  object, or character (i.e. a filepath)

- scenario:

  character, integer, or vector of these. Names or ids of one or more
  Scenarios. Note integer ids are slightly faster, but can only be used
  to open existing Scenarios

- sourceScenario:

  character or integer. If not `NULL` (Default), new Scenarios will be
  copies of the sourceScenario

- folder:

  [`Folder`](https://syncrosim.github.io/rsyncrosim/reference/Folder-class.md)
  object, character, or integer. The Folder object, name (must be
  unique), or Folder ID. If not `NULL` (Default), new Scenarios will be
  moved into the specified folder

- summary:

  logical. If `TRUE` then loads and returns the Scenario(s) in a named
  vector/dataframe with the scenarioId, name, description, owner,
  dateModified, readOnly, parentID. Default is `TRUE` if
  `scenario=NULL`, `FALSE` otherwise

- results:

  logical. If `TRUE` only return result Scenarios. Default is `FALSE`

- forceElements:

  logical. If `TRUE` then returns a single Scenario as a named list; if
  `FALSE` (default), returns a single Scenario as a
  [`Scenario`](https://syncrosim.github.io/rsyncrosim/reference/Scenario-class.md)
  object. Applies only when `summary=FALSE`

- overwrite:

  logical. If `TRUE` an existing Scenario will be overwritten. Default
  is `FALSE`

## Value

A `Scenario` object representing a SyncroSim scenario, a list of
Scenario objects, or a data frame of Scenario names and descriptions. If
`summary = FALSE`, returns one or more
[`Scenario`](https://syncrosim.github.io/rsyncrosim/reference/Scenario-class.md)
objects representing SyncroSim Scenarios. If `summary = TRUE`, returns
Scenario summary info.

## Details

For each element of Scenario:

- If element/Project/SsimObject uniquely identifies an existing
  Scenario: Returns the existing Scenario.

- If element/Project/SsimObject uniquely identifies more than one
  existing Scenario: Error.

- If element/Project/SsimObject do not identify an existing Scenario or
  Project: Error.

- If element/Project/SsimObject do not identify an existing Scenario and
  element is numeric: Error - a name is required for new Scenarios.
  SyncroSim will automatically assign an id when a Scenario is created.

- If element/Project/SsimObject do not identify an existing Scenario and
  do identify a Project, and element is a character string: Creates a
  new Scenario named element in the Project. SyncroSim automatically
  assigns an id. If sourceScenario is not `NULL` the new Scenario will
  be a copy of sourceScenario.

## Examples

``` r
# \donttest{
# Set the file path and name of the new SsimLibrary
myLibraryName <- file.path(tempdir(),"testlib")

# Set the SyncroSim Session, SsimLibrary, and Project
mySession <- session()
myLibrary <- ssimLibrary(name = myLibraryName, session = mySession) 
myProject <- project(myLibrary, project = "My Project")

# Create a new Scenario
myScenario <- scenario(myProject, scenario = "My Scenario")


# Create a new Scenario from an existing Scenario
myScenarioCopy <- scenario(myProject, scenario = "My Scenario Copy",
                           sourceScenario = myScenario)
                          
# Find all the Scenarios in a SsimLibrary
scenario(myLibrary)
#>   ScenarioId ProjectId ParentId             Name Owner MergeDependencies
#> 1          1         1       NA My Scenario Name   N/A               Yes
#> 2          2         1       NA      My Scenario   N/A                No
#> 3          3        18       NA      My Scenario   N/A                No
#> 4          4        18       NA My Scenario Copy   N/A                No
#>                        IgnoreDependencies IsResult IsReadOnly
#> 1 stsim_RunControl,stsim_TransitionTarget       No         No
#> 2                                               No        Yes
#> 3                                               No         No
#> 4                                               No         No
#>        DateLastModified
#> 1 2026-03-10 at 5:17 PM
#> 2 2026-03-10 at 5:17 PM
#> 3 2026-03-10 at 5:18 PM
#> 4 2026-03-10 at 5:18 PM

# Only return the results Scenarios for a SsimLibrary
scenario(myLibrary, results = TRUE)
#>  [1] ScenarioId         ProjectId          ParentId           Name              
#>  [5] Owner              MergeDependencies  IgnoreDependencies IsResult          
#>  [9] IsReadOnly         DateLastModified  
#> <0 rows> (or 0-length row.names)

# Overwrite an existing Scenario
myNewScenario <- scenario(myProject, scenario = "My New Scenario", 
                         overwrite = TRUE)
# }
```
