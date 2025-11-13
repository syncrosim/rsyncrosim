# Delete Project, Scenario, Folder, Chart or Datasheet

Delete Project, Scenario, Folder, Chart or Datasheet

## Usage

``` r
delete(
  ssimObject,
  project = NULL,
  scenario = NULL,
  folder = NULL,
  chart = NULL,
  datasheet = NULL,
  force = FALSE,
  session = NULL
)

# S4 method for class 'SsimObject'
delete(
  ssimObject,
  project = NULL,
  scenario = NULL,
  folder = NULL,
  chart = NULL,
  datasheet = NULL,
  force = FALSE,
  session = NULL
)
```

## Arguments

- ssimObject:

  `SsimLibrary`, `Project`, `Scenario`, `Folder`, or `Chart` object

- project:

  character string, numeric, or vector of these. One or more `Project`
  names or ids. Note that project argument is ignored if ssimObject is a
  list. Note that integer ids are slightly faster (optional)

- scenario:

  character string, numeric, or vector of these. One or more `Scenario`
  names or ids. Note that scenario argument is ignored if ssimObject is
  a list. Note that integer ids are slightly faster (optional)

- folder:

  character string, numeric, or vector of these. One or more `Folder`
  names or ids. Note that folder argument is ignored if ssimObject is a
  list. Note that integer ids are slightly faster (optional)

- chart:

  character string, numeric, or vector of these. One or more `Chart`
  names or ids. Note that chart argument is ignored if SsimObject is a
  list. Note that integer ids are slightly faster (optional)

- datasheet:

  character string or vector of these. One or more datasheet names
  (optional)

- force:

  logical. If `FALSE` (default), user will be prompted to approve
  removal of each item

- session:

  `Session` object. If `NULL` (default), session() will be used. Only
  applicable when `ssimObject` argument is a character

## Value

Invisibly returns a list of boolean values corresponding to each input:
`TRUE` upon success (i.e.successful deletion) and `FALSE` upon failure.

## Details

Deletes one or more items. Note that this is irreversible. To delete a
library, you must use the
[`deleteLibrary`](https://syncrosim.github.io/rsyncrosim/reference/deleteLibrary.md)
function instead.

## Examples

``` r
# \donttest{
# Specify file path and name of new SsimLibrary
myLibraryName <- file.path(tempdir(), "testlib")

# Set up a SyncroSim Session, SsimLibrary, and Project
mySession <- session()
myLibrary <- ssimLibrary(name = myLibraryName, session = mySession)
myProject <- project(myLibrary, project = "a project")

# Check the Projects associated with this SsimLibrary
project(myLibrary)
#>   ProjectId        Name Owner IsReadOnly IsActive       DateLastModified
#> 2        16   a project   N/A         No       No 2025-11-12 at 10:20 AM
#> 1         1 Definitions   N/A         No       No 2025-11-12 at 10:20 AM

# Delete Project
delete(myLibrary, project = "a project", force = TRUE)
#> Project 16 deleted

# Check that Project was successfully deleted from SsimLibrary
project(myLibrary)
#>   ProjectId        Name Owner IsReadOnly IsActive       DateLastModified
#> 1         1 Definitions   N/A         No       No 2025-11-12 at 10:20 AM
# }
```
