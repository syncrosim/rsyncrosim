# Create or open a chart

Create or open a
[`Chart`](https://syncrosim.github.io/rsyncrosim/reference/Chart-class.md)
from a SyncroSim
[`Project`](https://syncrosim.github.io/rsyncrosim/reference/Project-class.md).

## Usage

``` r
chart(ssimObject = NULL, chart = NULL, create = FALSE, summary = FALSE)

# S4 method for class 'character'
chart(ssimObject = NULL, chart = NULL, create = FALSE, summary = FALSE)

# S4 method for class 'SsimObject'
chart(ssimObject = NULL, chart = NULL, create = FALSE, summary = FALSE)
```

## Arguments

- ssimObject:

  [`Project`](https://syncrosim.github.io/rsyncrosim/reference/Project-class.md)
  or
  [`Scenario`](https://syncrosim.github.io/rsyncrosim/reference/Scenario-class.md)
  object

- chart:

  character or integer. If character, then will either open an existing
  chart if `create=FALSE`, or will create a new chart with the given
  name if the chart does not exist yet or `create=TRUE`. If integer,
  will open the existing chart with the given chart ID (if the ID
  exists). If no value is provided and `create=TRUE`, a new chart will
  be created with the default naming convention (e.g. "\_Chart1",
  "\_Chart2")

- create:

  logical. Whether to create a new chart if the chart name given already
  exists in the SyncroSim library. If `FALSE` (Default), then will
  return the existing chart with the given name. If `TRUE`, then will
  return a new chart with the same name as an existing chart (but
  different chart ID)

- summary:

  logical. If `TRUE`, returns a summary of chart information as an R
  data.frame. If `FALSE` (Default), then returns a SyncroSim Chart
  object

## Value

A `Chart` object representing a SyncroSim chart

## Examples

``` r
# \donttest{
# Set the file path and name of the new SsimLibrary
myLibraryName <- file.path(tempdir(),"testlib")

# Set the SyncroSim Session, SsimLibrary, Project, and Scenario
mySession <- session()
myLibrary <- ssimLibrary(name = myLibraryName, 
                         session = mySession,
                         packages = "stsim") 
#> Package <stsim v4.5.3> added
myProject <- project(myLibrary, project = "My Project")
myScenario <- scenario(myProject, scenario = "My Scenario")

# Create a new chart
myChart <- chart(myProject, chart = "New Chart")
# }
```
