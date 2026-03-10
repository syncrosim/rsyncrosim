# Retrieves printCmd setting of a Session

Retrieves a printCmd setting of a
[`Session`](https://syncrosim.github.io/rsyncrosim/reference/Session-class.md)
object. The printCmd setting configures a Session for printing commands
sent to the console.

## Usage

``` r
printCmd(session = NULL)

# S4 method for class 'Session'
printCmd(session = NULL)

# S4 method for class 'missingOrNULLOrChar'
printCmd(session = NULL)
```

## Arguments

- session:

  Session object or character. The Session or path to a Session where
  the printCmd settings are retrieved from. If `NULL` (default),
  [`session()`](https://syncrosim.github.io/rsyncrosim/reference/session.md)
  will be used

## Value

A logical : `TRUE` if the session is configured to print commands and
`FALSE` if it is not.

## Examples

``` r
# \donttest{
# Set SyncroSim Session
mySession <- session()

# Retrieve printCmd settings for given Session
printCmd(mySession)
#> [1] FALSE
# }
```
