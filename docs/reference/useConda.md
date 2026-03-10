# Conda configuration of a SsimLibrary

## Usage

``` r
useConda(ssimObject)

# S4 method for class 'character'
useConda(ssimObject)

# S4 method for class 'SsimLibrary'
useConda(ssimObject)

useConda(ssimObject) <- value

# S4 method for class 'logical'
useConda(ssimObject) <- value

# S4 method for class 'SsimLibrary'
useConda(ssimObject) <- value
```

## Arguments

- value:

  logical for whether to use Conda environments for the given SyncroSim
  Library. If set to `TRUE`, then Conda environments will be used. If
  set to `FALSE`, then Conda environments will not be used during
  runtime.

## Value

Logical: whether Conda environments will be used during runtime for the
given

## Examples

``` r
if (FALSE) { # \dontrun{
# Set up a SyncroSim Session, SsimLibrary
mySession <- session()

# Retrieve Conda configuration status of the SsimLibrary
useConda(myLibrary)

# Set the Conda configuration of the SyncroSim Library
useConda(myLibrary) <- TRUE

# Only use Conda with the specified SyncroSim packages
useConda(myLibrary) <- "helloworld"

# Only use Conda with multiple specified SyncroSim packages
useConda(myLibrary) <- c("helloworld", "stsim")
} # }
```
