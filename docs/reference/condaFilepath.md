# Path to Conda installation folder

Gets or sets the path to the Conda installation folder. Can be used to
direct SyncroSim to a custom Conda installation.

## Usage

``` r
condaFilepath(session)

# S4 method for class 'Session'
condaFilepath(session)

# S4 method for class 'missingOrNULLOrChar'
condaFilepath(session)

condaFilepath(session) <- value

# S4 method for class 'character'
condaFilepath(session) <- value

# S4 method for class 'Session'
condaFilepath(session) <- value
```

## Arguments

- session:

  `Session` object or character (i.e. filepath to a session). If `NULL`,
  [`session()`](https://syncrosim.github.io/rsyncrosim/reference/session.md)
  will be used

- value:

  character. If empty, then returns the current Conda installation path

## Value

A character: the currently set filepath of the Conda installation
folder.

## Examples

``` r
if (FALSE) { # \dontrun{
# Set up a SyncroSim Session
mySession <- session()

# Retrieve Conda installation path for the SyncroSim Session
condaFilepath(mySession)

# Set the Conda installation path for the SyncroSim Session
condaFilepath(mySession) <- "C:/miniconda3"
} # }
```
