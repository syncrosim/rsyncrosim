# Installs Miniforge or Miniconda

This function installs the Miniforge or Miniconda package manager
software to the default installation path within the SyncroSim
installation folder. If you already have conda installed in the
non-default location, you can point SyncroSim towards that installation
using the
[`condaFilepath`](https://syncrosim.github.io/rsyncrosim/reference/condaFilepath.md)
function.

## Usage

``` r
installConda(session, software = "miniforge")

# S4 method for class 'character'
installConda(session, software = "miniforge")

# S4 method for class 'missingOrNULL'
installConda(session, software = "miniforge")

# S4 method for class 'Session'
installConda(session, software = "miniforge")
```

## Arguments

- session:

  [`Session`](https://syncrosim.github.io/rsyncrosim/reference/Session-class.md)
  object. If `NULL` (default),
  [`session()`](https://syncrosim.github.io/rsyncrosim/reference/session.md)
  will be used

- software:

  character. Whether to install the latest release of "miniforge"
  (Default) or "miniconda".

## Value

Invisibly returns `TRUE` upon success (i.e.successful install) and
`FALSE` upon failure.

## Examples

``` r
if (FALSE) { # \dontrun{
# Install miniforge for the default SyncroSim session
installConda()

# Install miniconda for the default SyncroSim session
installConda(software = "miniconda")
} # }
```
