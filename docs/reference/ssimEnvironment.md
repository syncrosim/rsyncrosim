# SyncroSim Environment

This function is part of a set of functions designed to facilitate the
development of R-based Syncrosim Packages. `ssimEnvironment` retrieves
specific environment variables.

## Usage

``` r
ssimEnvironment()
```

## Value

Returns a single-row data.frame of SyncroSim specific environment
variables.

## Examples

``` r
if (FALSE) { # \dontrun{
# Get the whole set of variables
e <- ssimEnvironment()

# Get the path to transfer directory, for instance
transferdir <- e$TransferDirectory
} # }
```
