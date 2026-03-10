# Conda configuration of a SsimLibrary

Retrieves or sets the Conda configuration of a
[`SsimLibrary`](https://syncrosim.github.io/rsyncrosim/reference/SsimLibrary-class.md).
Note that in order to use conda environments, you will first need to
ensure that the conda environment has been created for a given package.
You can create the conda environment for a package using the
[`createCondaEnv`](https://syncrosim.github.io/rsyncrosim/reference/createCondaEnv.md)
function.

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

- ssimObject:

  [`SsimLibrary`](https://syncrosim.github.io/rsyncrosim/reference/SsimLibrary-class.md)
  object

- value:

  logical for whether to use Conda environments for the given SyncroSim
  Library. If set to `TRUE`, then Conda environments will be used. If
  set to `FALSE`, then Conda environments will not be used during
  runtime.

## Value

Logical: whether Conda environments will be used during runtime for the
given
[`SsimLibrary`](https://syncrosim.github.io/rsyncrosim/reference/SsimLibrary-class.md)

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
