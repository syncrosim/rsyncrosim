# Function to write to the SyncroSim run log

This function is designed to facilitate the development of R-based
Syncrosim Packages by allowing developers to send messages to the run
log.

## Usage

``` r
updateRunLog(..., sep = "", type = "status")
```

## Arguments

- ...:

  One or more objects which can be coerced to character which are pasted
  together using `sep`.

- sep:

  character. Used to separate terms. Not NA_character\_

- type:

  character. Type of message to add to run log. One of "status",
  (default) "info", or "warning".

## Value

No returned value, used for side effects

## Examples

``` r
if (FALSE) { # \dontrun{
# Write a message to run log
updateRunLog(msg)

# Construct and write a message to run log
updateRunLog(msg, additionalMsg, sep = " ")
} # }
```
