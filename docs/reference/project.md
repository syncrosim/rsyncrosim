# Create or open Project(s)

Creates or retrieves a
[`Project`](https://syncrosim.github.io/rsyncrosim/reference/Project-class.md)
or multiple Projects from a SsimLibrary.

## Usage

``` r
project(
  ssimObject = NULL,
  project = NULL,
  sourceProject = NULL,
  summary = NULL,
  forceElements = FALSE,
  overwrite = FALSE
)
```

## Arguments

- project:

  [`Project`](https://syncrosim.github.io/rsyncrosim/reference/Project-class.md)
  object, character, integer, or vector of these. Names or ids of one or
  more Projects. Note that integer ids are slightly faster (optional)

- sourceProject:

  [`Project`](https://syncrosim.github.io/rsyncrosim/reference/Project-class.md)
  object, character, or integer. If not `NULL` (default), new Projects
  will be copies of the sourceProject

- summary:

  logical. If `TRUE` then return the Project(s) in a data.frame with the
  projectId, name, description, owner, dateModified, readOnly. Default
  is `TRUE` if `project=NULL` and SsimObject is not Scenario/Project,
  `FALSE` otherwise

- forceElements:

  logical. If `TRUE` then returns a single Project as a named list;
  otherwise returns a single project as a
  [`Project`](https://syncrosim.github.io/rsyncrosim/reference/Project-class.md)
  object. Applies only when `summary=FALSE` Default is `FALSE`

- overwrite:

  logical. If `TRUE` an existing Project will be overwritten. Default is
  `FALSE`

## Value

Returns a
[`Project`](https://syncrosim.github.io/rsyncrosim/reference/Project-class.md)
object representing a SyncroSim Project. If summary is `TRUE`, returns a
data.frame of Project names and descriptions.

## Details

For each element of project:

- If element identifies an existing Project: Returns the existing
  Project.

- If element identifies more than one Project: Error.

- If element does not identify an existing Project: Creates a new
  Project named element. Note that SyncroSim automatically assigns an id
  to a new Project.

## Examples

``` r
# \donttest{
# Set the file path and name of the new SsimLibrary
myLibraryName <- file.path(tempdir(),"testlib_project")

# Set the SyncroSim Session, SsimLibrary, and Project
mySession <- session()
myLibrary <- ssimLibrary(name = myLibraryName, session = mySession) 
myProject <- project(ssimObject = myLibrary, project = "My project name")
myproject2 <- project(ssimObject = myLibrary, project = "My new project name")

# Get a named list of existing Projects
# Each element in the list is named by a character version of the Project ID
myProjects <- project(myLibrary, summary = FALSE)
names(myProjects)
#> [1] "7" "1"

# Get an existing Project.
myProject <- myProjects[[1]]
myProject <- project(myLibrary, project = "My new project name")

# Get/set the Project properties
name(myProject)
#> [1] "My new project name"
name(myProject) <- "New project name"

# Create a new Project from a copy of an existing Project
myNewProject <- project(myLibrary, project = "My copied project",
                        sourceProject = 1)

# Overwrite an existing Project
myNewProject <- project(myLibrary, project = "My copied project",
                        overwrite = TRUE)
# }
```
