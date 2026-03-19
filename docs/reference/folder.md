# Create or open a Folder

Create or open a
[`Folder`](https://syncrosim.github.io/rsyncrosim/reference/Folder-class.md)
from a SyncroSim
[`Project`](https://syncrosim.github.io/rsyncrosim/reference/Project-class.md).

## Usage

``` r
folder(
  ssimObject = NULL,
  folder = NULL,
  parentFolder = NULL,
  summary = FALSE,
  create = FALSE
)
```

## Arguments

- ssimObject:

  [`SsimLibrary`](https://syncrosim.github.io/rsyncrosim/reference/SsimLibrary-class.md)
  or
  [`Project`](https://syncrosim.github.io/rsyncrosim/reference/Project-class.md)
  object.

- folder:

  character or integer. If character, then will either open an existing
  folder if `create=FALSE`, or will create a new folder with the given
  name if the folder does not exist yet or `create=TRUE` (Default). If
  integer, will open the existing folder with the given folder ID (if
  the ID exists).

- parentFolder:

  character, integer, or SyncroSim Folder object. If not `NULL`
  (Default), the new folder will be created inside of the specified
  parent folder

- summary:

  logical. If `FALSE`, then returns a folder object. If `TRUE`, then
  returns a dataframe of information about the specified folder

- create:

  logical. Whether to create a new folder if the folder name given
  already exists in the SyncroSim library. If `FALSE` (Default), then
  will return the existing folder with the given name. If `TRUE`, then
  will return a new folder with the same name as an existing folder (but
  different folder ID)

## Value

A `Folder` object representing a SyncroSim folder.

## Examples

``` r
# \donttest{
# Set the file path and name of the new SsimLibrary
myLibraryName <- file.path(tempdir(),"testlib")

# Set the SyncroSim Session, SsimLibrary, Project, and Scenario
mySession <- session()
myLibrary <- ssimLibrary(name = myLibraryName, session = mySession) 
myProject <- project(myLibrary, project = "My Project")
myScenario <- scenario(myProject, scenario = "My Scenario")

# Create a new folder
myFolder <- folder(myProject, folder = "New Folder")

# Create a nested folder within "New Folder"
myNestedFolder <- folder(myProject, folder = "New Nested Folder", 
                         parentFolder = myFolder)
                         
# Retrieve a dataframe of all folders in a project
folder(myProject)
#>   FolderId              Name ProjectId ParentId Owner IsReadOnly
#> 1       46        New Folder        31      N/A   N/A         No
#> 2       47 New Nested Folder        31       46   N/A         No
#>            LastModified
#> 1 2026-03-19 at 1:12 PM
#> 2 2026-03-19 at 1:12 PM
# }
```
