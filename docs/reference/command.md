# SyncroSim console command

This function issues a command to the SyncroSim console, and is mostly
used internally by other functions.

## Usage

``` r
command(
  args,
  session = NULL,
  program = "SyncroSim.Console.exe",
  wait = TRUE,
  progName = NULL
)
```

## Arguments

- args:

  character string, named list, named vector, unnamed list, or unnamed
  vector. Arguments for the SyncroSim console. See 'details' for more
  information about this argument

- session:

  [`Session`](https://syncrosim.github.io/rsyncrosim/reference/Session-class.md)
  object. If `NULL`(default), the default session will be used

- program:

  character. The name of the target SyncroSim executable. Options
  include "SyncroSim.Console.exe" (default), "SyncroSim.VizConsole.exe",
  "SyncroSim.PackageManager.exe" and "SyncroSim.Multiband.exe"

- wait:

  logical. If `TRUE`(default) R will wait for the command to finish
  before proceeding. Note that silent(session) is ignored if
  `wait=FALSE`

- progName:

  character. Internal argument for setting path to SyncroSim
  installation folder.

## Value

Character string: output from the SyncroSim program.

## Details

Example args, and the resulting character string passed to the SyncroSim
console:

- Character string e.g. "–create –help": "–create –help"

- Named list or named vector e.g. list(name1=NULL,name2=value2): "–name1
  –name2=value2"

- Unnamed list or unnamed vector e.g. c("create","help"): "–create
  –help"

## Examples

``` r
if (FALSE) { # \dontrun{
# Install "stsim" if not already installed
installPackage("stsim")

# Set the file path and name of the new SsimLibrary
myLibraryName <- file.path(tempdir(),"testlib.ssim")

# Specify the command line arguments for creating a new stsim SsimLibrary
args <- list(create = NULL, library = NULL, 
             name = myLibraryName, 
             package = "stsim")
        
# Use a default session to create a new SsimLibrary in the current working directory
output <- command(args, session = session(printCmd = TRUE))
output

# Provide arguments to the command line using an unnamed vector
command(c("create", "help"))

# Provide arguments to the command line using a character string
command("--create --help")

# Provide arguments to the command line using a named list
command(list(create = NULL, help = NULL))

# Call on a different program to find all installed packages
command(list(installed = NULL), program = "SyncroSim.PackageManager.exe")
} # }
```
