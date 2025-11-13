# Signs out of SyncroSim

Signs out of syncrosim.com.

## Usage

``` r
signOut(session = NULL)
```

## Arguments

- session:

  `Session` object. If `NULL`(default), the default session will be used

## Value

Character string: whether sign out was successful or not.

## Examples

``` r
if (FALSE) { # \dontrun{
# Sign out of SyncroSim session
mySession <- session()
signOut(mySession)
} # }
```
