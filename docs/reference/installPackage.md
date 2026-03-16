# Adds package to SyncroSim Installation

This function installs a package to the SyncroSim
[`Session`](https://syncrosim.github.io/rsyncrosim/reference/Session-class.md).
If only the package name is provided as input, the function queries the
SyncroSim package server for the specified package. If a file path is
provided as input, the function installs a package to SyncroSim from a
local package file (ends in ".ssimpkg"). The list of SyncroSim packages
can be found [here](https://syncrosim.com/packages/).

## Usage

``` r
installPackage(packages, versions = NULL, session = NULL)

# S4 method for class 'ANY,ANY,character'
installPackage(packages, versions = NULL, session = NULL)

# S4 method for class 'ANY,ANY,missingOrNULL'
installPackage(packages, versions = NULL, session = NULL)

# S4 method for class 'ANY,ANY,Session'
installPackage(packages, versions = NULL, session = NULL)
```

## Arguments

- packages:

  character string. The name or file path of the package to install

- versions:

  character string. The packages version(s) to install if installing a
  package from the server. If `NULL` then installs the latest version

- session:

  [`Session`](https://syncrosim.github.io/rsyncrosim/reference/Session-class.md)
  object. If `NULL` (default),
  [`session()`](https://syncrosim.github.io/rsyncrosim/reference/session.md)
  will be used

## Value

Invisibly returns `TRUE` upon success (i.e.successful install) and
`FALSE` upon failure.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create a new SyncroSim Session
mySession <- session()

# Install package from the package server
installPackage(packages="stsim", versions="4.0.1", session = mySession)

# Install package using a local file path
installPackage("c:/path/to/stsim.ssimpkg")
} # }
```
