# Silent status of SyncroSim Session

Checks or sets whether a SyncroSim `Session` is silent or not. In a
silent session, warnings from the console are ignored.

## Usage

``` r
silent(session)

# S4 method for class 'Session'
silent(session)

# S4 method for class 'missingOrNULLOrChar'
silent(session)

silent(session) <- value

# S4 method for class 'character'
silent(session) <- value

# S4 method for class 'Session'
silent(session) <- value
```

## Arguments

- session:

  `Session` object or character (i.e. filepath to a session). If `NULL`,
  [`session()`](https://syncrosim.github.io/rsyncrosim/reference/session.md)
  will be used

- value:

  logical. If `TRUE` (default), the SyncroSim Session will be silent

## Value

A logical: `TRUE` if the session is silent and `FALSE` otherwise.

## Examples

``` r
# \donttest{
# Set up a SyncroSim Session
mySession <- session()

# Check the silent status of a SyncroSim Session
silent(mySession)
#> [1] TRUE

# Set the silent status of a SyncroSim Session
silent(mySession) <- FALSE
# }
```
