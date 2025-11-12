# Display SyncroSim profile

Displays the currently signed in SyncroSim profile information. To sign
in to SyncroSim use the
[`signIn`](https://syncrosim.github.io/rsyncrosim/reference/signIn.md)
function.

## Usage

``` r
viewProfile(session = NULL, ...)
```

## Arguments

- session:

  `Session` object. If `NULL`(default), the default session will be used

- ...:

  other internal parameters

## Examples

``` r
if (FALSE) { # \dontrun{
# Retrieve profile information for a SyncroSim session
mySession <- session()
viewProfile(mySession)
} # }
```
