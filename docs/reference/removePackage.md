# Removes SyncroSim package(s)

Removes package(s) from a
[`SsimLibrary`](https://syncrosim.github.io/rsyncrosim/reference/SsimLibrary-class.md).

## Usage

``` r
removePackage(ssimLibrary, packages)

# S4 method for class 'character'
removePackage(ssimLibrary, packages)

# S4 method for class 'SsimLibrary'
removePackage(ssimLibrary, packages)
```

## Arguments

- ssimLibrary:

  [`SsimLibrary`](https://syncrosim.github.io/rsyncrosim/reference/SsimLibrary-class.md)
  object

- packages:

  character string or vector of package name(s)

## Value

This function invisibly returns `TRUE` upon success (i.e.successful
removal of the package) or `FALSE` upon failure.

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

# Remove package
removePackage(myLibrary, packages = c("stsim", "stsimecodep"))
packages(myLibrary)
} # }
```
