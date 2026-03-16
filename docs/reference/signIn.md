# Signs in to SyncroSim

Signs in to syncrosim.com to authenticate user credentials.

## Usage

``` r
signIn(session = NULL)
```

## Arguments

- session:

  [`Session`](https://syncrosim.github.io/rsyncrosim/reference/Session-class.md)
  object. If `NULL`(default), the default session will be used

## Value

Character string: whether sign in was successful or not.

## Examples

``` r
if (FALSE) { # \dontrun{
# Sign in to SyncroSim session
mySession <- session()
signIn(mySession)
} # }
```
