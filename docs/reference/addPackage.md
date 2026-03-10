# Add SyncroSim package(s)

Adds package(s) to a
[`SsimLibrary`](https://syncrosim.github.io/rsyncrosim/reference/SsimLibrary-class.md).

## Usage

``` r
addPackage(ssimLibrary, packages, versions = NULL, forceUpdate = FALSE)

# S4 method for class 'character'
addPackage(ssimLibrary, packages, versions = NULL, forceUpdate = FALSE)

# S4 method for class 'SsimLibrary'
addPackage(ssimLibrary, packages, versions = NULL, forceUpdate = FALSE)
```

## Arguments

- ssimLibrary:

  [`SsimLibrary`](https://syncrosim.github.io/rsyncrosim/reference/SsimLibrary-class.md)
  object

- packages:

  character string or vector of package name(s)

- versions:

  character string or vector of package version(s). If `NULL` then uses
  the latest installed version of the package

- forceUpdate:

  logical. If `FALSE` (default) user will be prompted to approve any
  required updates. If `TRUE`, required updates will be applied
  silently.

## Value

Invisibly returns `TRUE` upon success (i.e.successful addition of the
package) or `FALSE` upon failure.

## See also

[`packages`](https://syncrosim.github.io/rsyncrosim/reference/packages.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Install "stsim" and "stsimecodep" SyncroSim packages
installPackage(packages = c("stsim", "stsim"),
               versions = c("4.0.1", "4.3.5"))
installPackage("stsimecodep")

# Specify file path and name of new SsimLibrary
myLibraryName <- file.path(tempdir(), "testlib")

# Set up a SyncroSim Session, SsimLibrary, and Project
mySession <- session()
myLibrary <- ssimLibrary(name = myLibraryName, session = mySession)

# Add package
addPackage(myLibrary, packages = "stsim", versions = "4.0.1")
addPackage(myLibrary, packages = "stsimecodep")
packages(myLibrary)

# Change package version
addPackage(myLibrary, packages = "stsim", versions = "4.3.5")

# Remove package
removePackage(myLibrary, packages = c("stsim", "stsimecodep"))
packages(myLibrary)
} # }
```
