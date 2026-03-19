# Create or open a SsimLibrary

Creates or opens a
[`SsimLibrary`](https://syncrosim.github.io/rsyncrosim/reference/SsimLibrary-class.md)
object. If `summary = TRUE`, returns SsimLibrary summary info. If
`summary = NULL`, returns SsimLibrary summary info if ssimObject is a
SsimLibrary, SsimLibrary object otherwise.

## Usage

``` r
ssimLibrary(
  name = NULL,
  summary = NULL,
  packages = NULL,
  session = NULL,
  forceUpdate = FALSE,
  overwrite = FALSE,
  useConda = NULL
)

# S4 method for class 'SsimObject'
ssimLibrary(
  name = NULL,
  summary = NULL,
  packages = NULL,
  session = NULL,
  forceUpdate = FALSE,
  overwrite = FALSE,
  useConda = NULL
)

# S4 method for class 'missingOrNULLOrChar'
ssimLibrary(
  name = NULL,
  summary = NULL,
  packages = NULL,
  session = NULL,
  forceUpdate = FALSE,
  overwrite = FALSE,
  useConda = NULL
)
```

## Arguments

- name:

  [`SsimLibrary`](https://syncrosim.github.io/rsyncrosim/reference/SsimLibrary-class.md),
  [`Project`](https://syncrosim.github.io/rsyncrosim/reference/Project-class.md)
  or
  [`Scenario`](https://syncrosim.github.io/rsyncrosim/reference/Scenario-class.md)
  object, or character string (i.e. path to a SsimLibrary or SsimObject)

- summary:

  logical. Default is `TRUE`

- packages:

  character or character vector. The SyncroSim Package(s) to add to the
  Library if creating a new Library (optional)

- session:

  [`Session`](https://syncrosim.github.io/rsyncrosim/reference/Session-class.md)
  object. If `NULL` (default), session() will be used

- forceUpdate:

  logical. If `FALSE` (default) user will be prompted to approve any
  required updates. If `TRUE`, required updates will be applied
  silently.

- overwrite:

  logical. If `TRUE` an existing SsimLibrary will be overwritten

- useConda:

  logical. If set to TRUE, then all packages associated with the Library
  will have their Conda environments created and Conda environments will
  be used during runtime.If set to FALSE, then no packages will have
  their Conda environments created and Conda environments will not be
  used during runtime. Default is NULL

## Value

Returns a
[`SsimLibrary`](https://syncrosim.github.io/rsyncrosim/reference/SsimLibrary-class.md)
object.

## Details

Example arguments:

- If name is SyncroSim Project or Scenario: Returns the
  [`SsimLibrary`](https://syncrosim.github.io/rsyncrosim/reference/SsimLibrary-class.md)
  associated with the Project or Scenario.

- If name is `NULL`: Create/open a SsimLibrary in the current working
  directory with the filename SsimLibrary.ssim.

- If name is a string: If string is not a valid path treat as filename
  in working directory. If no file suffix provided in string then add
  .ssim. Attempts to open a SsimLibrary of that name. If SsimLibrary
  does not exist creates a SsimLibrary of type package in the current
  working directory.

- If given a name and a package: Create/open a SsimLibrary called
  [name](https://syncrosim.github.io/rsyncrosim/reference/name.md).ssim.
  Returns an error if the SsimLibrary already exists but is a different
  type of package.

## Examples

``` r
# \donttest{
# Make sure packages are installed
installPackage("stsim")
#> [1] "Package stsim v4.5.3 is already installed."

# Create or open a SsimLibrary using the default Session
myLibrary <- ssimLibrary(name = file.path(tempdir(), "mylib"))

# Create SsimLibrary using a specific Session
mySession <- session()

myLibrary <- ssimLibrary(name = file.path(tempdir(), "mylib"),
                         session = mySession)

# Retrieve SsimLibrary properties
session(myLibrary)
#> class               : Session
#> filepath [character]: C:\PROGRA~1\SYNCRO~1
#> silent [logical]    : TRUE
#> printCmd [logical]  : FALSE
#> condaFilepath [NULL]: 

# Create SsimLibrary from template
installPackage("helloworldSpatial")
#> [1] "Package helloworldSpatial v2.1.0 is already installed."
mySession <- session()
myLibrary <- ssimLibrary(name = file.path(tempdir(), "mylib"), 
                         session = mySession,
                         forceUpdate = TRUE,
                         packages = "helloworldSpatial",
                         overwrite = TRUE)
#> Library C:\Users\VICKIZ~1\AppData\Local\Temp\RtmpWUJoYX/mylib.ssim deleted
#> Package <helloworldSpatial v2.1.0> added
                         
# }
```
