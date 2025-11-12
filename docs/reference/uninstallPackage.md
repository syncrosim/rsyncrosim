# Removes a package from SyncroSim installation

Removes a package from SyncroSim installation

## Usage

``` r
uninstallPackage(packages, versions = NULL, session = NULL)

# S4 method for class 'ANY,ANY,character'
uninstallPackage(packages, versions = NULL, session = NULL)

# S4 method for class 'ANY,ANY,missingOrNULL'
uninstallPackage(packages, versions = NULL, session = NULL)

# S4 method for class 'ANY,ANY,Session'
uninstallPackage(packages, versions = NULL, session = NULL)
```

## Arguments

- packages:

  character or character vector. The name(s) of the package(s) to
  uninstall

- versions:

  character or character vector. The version(s) of the package(s) to
  uninstall. If `NULL` then will uninstall all versions of the
  package(s).

- session:

  `Session` object. If `NULL` (default),
  [`session()`](https://syncrosim.github.io/rsyncrosim/reference/session.md)
  will be used

## Value

Invisibly returns `TRUE` upon success (i.e.successful removal) and
`FALSE` upon failure.

## Examples

``` r
if (FALSE) { # \dontrun{
# Set SyncroSim session
mySession <- session()

# Install packages to SyncroSim session
installPackages(packages = c("stsim", "stsim"),
                versions = c("4.0.1", "4.3.5"))

# Uninstalls specific version of package from SyncroSim session
uninstallPackage(packages = "stsim", versions = "4.0.1", session = mySession)

# Uninstalls all instances ofa package from SyncroSim session
uninstallPackage(packages = "stsim", session = mySession)
} # }
```
