# Retrieves SyncroSim version

Retrieves the version of a SyncroSim Session.

## Usage

``` r
version(session = NULL)

# S4 method for class 'character'
version(session = NULL)

# S4 method for class 'missingOrNULL'
version(session = NULL)

# S4 method for class 'Session'
version(session = NULL)
```

## Arguments

- session:

  `Session` object

## Value

A character string e.g. "2.2.13".

## Examples

``` r
# \donttest{
# Set SyncroSim Session
mySession <- session()

# Retrieve version of SyncroSim associated with Session
version(mySession)
#> [1] "3.1.20"
# }
```
