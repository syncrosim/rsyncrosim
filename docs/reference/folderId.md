# Retrieves folderId of SyncroSim Folder or Scenario

Retrieves the Folder Id of a SyncroSim
[`Folder`](https://syncrosim.github.io/rsyncrosim/reference/Folder-class.md)
or
[`Scenario`](https://syncrosim.github.io/rsyncrosim/reference/Scenario-class.md).
Can also use to set the Folder Id for a
[`Scenario`](https://syncrosim.github.io/rsyncrosim/reference/Scenario-class.md) -
this will move the
[`Scenario`](https://syncrosim.github.io/rsyncrosim/reference/Scenario-class.md)
into the desired folder in the SyncroSim User Interface.

## Usage

``` r
folderId(ssimObject)

# S4 method for class 'character'
folderId(ssimObject)

# S4 method for class 'Folder'
folderId(ssimObject)

# S4 method for class 'Scenario'
folderId(ssimObject)

folderId(ssimObject) <- value

# S4 method for class 'Scenario'
folderId(ssimObject) <- value
```

## Arguments

- ssimObject:

  [`Folder`](https://syncrosim.github.io/rsyncrosim/reference/Folder-class.md)
  or
  [`Scenario`](https://syncrosim.github.io/rsyncrosim/reference/Scenario-class.md)
  object

- value:

  integer of the folder ID to move the
  [`Scenario`](https://syncrosim.github.io/rsyncrosim/reference/Scenario-class.md)
  to. Only applicable if the ssimObject provided is a
  [`Scenario`](https://syncrosim.github.io/rsyncrosim/reference/Scenario-class.md).

## Value

An integer: folder id.

## Examples

``` r
# \donttest{
# Set the file path and name of the new SsimLibrary
myLibraryName <- file.path(tempdir(),"testlib")

# Set the SyncroSim Session, SsimLibrary, Project, and Scenario
mySession <- session()
myLibrary <- ssimLibrary(name = myLibraryName, 
                         session = mySession, 
                         overwrite = TRUE) 
#> Library C:\Users\VICKIZ~1\AppData\Local\Temp\Rtmpqq1REa/testlib.ssim deleted
myProject <- project(myLibrary, project = "Definitions")
myScenario <- scenario(myProject, scenario = "My Scenario")
myFolder <- folder(myProject, "New Folder")

# Get Folder ID for SyncroSim Folder and Scenario
folderId(myFolder)
#> [1] 7
folderId(myScenario)
#> [1] NA

# Move the Scenario into the newly created folder
folderId(myScenario) <- folderId(myFolder)
folderId(myScenario)
#> [1] 7
# }
```
