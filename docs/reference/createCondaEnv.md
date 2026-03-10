# Create SyncroSim package conda environments

Creates the conda environment for the specified SyncroSim package(s).

## Usage

``` r
createCondaEnv(pkgs, session = NULL)

# S4 method for class 'ANY,character'
createCondaEnv(pkgs, session = NULL)

# S4 method for class 'ANY,missingOrNULL'
createCondaEnv(pkgs, session = NULL)

# S4 method for class 'ANY,Session'
createCondaEnv(pkgs, session = NULL)
```

## Arguments

- pkgs:

  character or list of characters.

- session:

  [`Session`](https://syncrosim.github.io/rsyncrosim/reference/Session-class.md)
  object or character (i.e. filepath to a session). If `NULL`,
  [`session()`](https://syncrosim.github.io/rsyncrosim/reference/session.md)
  will be used

## Value

Invisibly returns `TRUE` upon success (i.e.successful creation of the
conda environment(s)) or `FALSE` upon failure.

## Examples

``` r
if (FALSE) { # \dontrun{
# Set up a SyncroSim Session
mySession <- session()

# Create the conda environment for helloworldConda package
condaFilepath(pkgs = "helloworldConda", mySession)
} # }
```
