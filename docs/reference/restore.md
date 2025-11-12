# Restore Library

Restores a SyncroSim library from a backup file.

## Usage

``` r
restore(ssimLibraryBackup, folder = NULL, session = NULL)

# S4 method for class 'character'
restore(ssimLibraryBackup, folder = NULL, session = NULL)
```

## Arguments

- ssimLibraryBackup:

  character string. Path to a library backup file

- folder:

  character string. Optional path to a folder to restore the library
  into. If NULL, restores to default location. If the folder specified
  does not exist, it will be created.

- session:

  SyncroSim session.

## Value

Invisibly returns `TRUE` upon success (i.e. successful restore) and
`FALSE` upon failure.

## Examples

``` r
if (FALSE) { # \dontrun{
# Specify file path and name of SsimLibrary backup file
myLibraryBackupName <- file.path(tempdir(), "testlib.ssimbak")

# Set up a SyncroSim Session and restore SsimLibrary from backup file
mySession <- session()
restore(ssimLibraryBackup = myLibraryBackupName,
        session = mySession)

} # }
```
