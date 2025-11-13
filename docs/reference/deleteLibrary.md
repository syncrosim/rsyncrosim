# Delete Library

Deletes a SyncroSim library. Note this is irreversable.

## Usage

``` r
deleteLibrary(
  ssimLibrary,
  force = FALSE,
  removeBackup = FALSE,
  removePublish = FALSE,
  removeCustom = FALSE,
  session = NULL
)

# S4 method for class 'SsimLibrary'
deleteLibrary(ssimLibrary, force, removeBackup, removePublish, removeCustom)

# S4 method for class 'character'
deleteLibrary(
  ssimLibrary,
  force = FALSE,
  removeBackup = FALSE,
  removePublish = FALSE,
  removeCustom = FALSE,
  session = NULL
)
```

## Arguments

- ssimLibrary:

  SsimLibrary or path to a library

- force:

  Logical. If FALSE (default) prompt to confirm that the library should
  be deleted. This is irreversable.

- removeBackup:

  logical. If `TRUE`, will remove the backup folder when deleting a
  library. Default is FALSE.

- removePublish:

  logical. If TRUE, will remove the publish folder when deleting a
  library. Default is FALSE.

- removeCustom:

  logical. If TRUE and custom folders have been configured for a
  library, then will remove the custom publish and/or backup folders
  when deleting a library. Note that the `removePublish` and
  `removeBackup` arguments must also be set to TRUE to remove the
  respective custom folders. Default is FALSE.

- session:

  Session

## Value

"saved" or failure message.

## Examples

``` r
if (FALSE) { # \dontrun{
# Specify file path and name of new SsimLibrary
myLibraryName <- file.path(tempdir(), "testlib")
myLibraryName2 <- file.path(tempdir(), "testlib2")

# Set up a SyncroSim Session and create SsimLibrary
mySession <- session()
myLibrary <- ssimLibrary(name = myLibraryName, session = mySession)
 
# Delete library from SsimObject
deleteLibrary(myLibrary, force = TRUE, removeBackup = TRUE)

# Create another library
myLibrary <- ssimLibrary(name = myLibraryName2, session = mySession)

# Delete library from path
deleteLibrary(myLibraryName2)
} # }
```
