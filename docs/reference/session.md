# Create or return SyncroSim Session

Methods to create or return a SyncroSim `Session`.

## Usage

``` r
session(x = NULL, silent = TRUE, printCmd = FALSE)

# S4 method for class 'missingOrNULLOrChar'
session(x = NULL, silent = TRUE, printCmd = FALSE)

# S4 method for class 'SsimObject'
session(x = NULL, silent = TRUE, printCmd = FALSE)

# S4 method for class 'Folder'
session(x = NULL, silent = TRUE, printCmd = FALSE)

session(ssimObject) <- value

# S4 method for class 'NULLOrChar'
session(ssimObject) <- value

# S4 method for class 'SsimObject'
session(ssimObject) <- value
```

## Arguments

- x:

  character or SsimObject. Path to SyncroSim installation. If `NULL`
  (default), then default path is used

- silent:

  logical. Applies only if x is a path or `NULL` If `TRUE`, warnings
  from the console are ignored. Otherwise they are printed. Default is
  `FALSE`

- printCmd:

  logical. Applies only if x is a path or `NULL` If `TRUE`, arguments
  passed to the SyncroSim console are also printed. Helpful for
  debugging. Default is `FALSE`

- ssimObject:

  `Project` or `Scenario` object

- value:

  `Session` object

## Value

A SyncroSim `Session` object.

## Details

In order to avoid problems with SyncroSim version compatibility and
SsimLibrary updating, the new Session must have the same filepath as the
Session of the SsimObject e.g.
`filepath(value)==filepath(session(ssimObject))`. Therefore, the only
time when you will need to set a new SyncroSim Session is if you have
updated the SyncroSim software and want to update an existing SsimObject
to use the new software.

## Examples

``` r
# \donttest{
# Specify file path and name of new SsimLibrary
myLibraryName <- file.path(tempdir(), "testlib")

# Set up a SyncroSim Session, SsimLibrary, and Project
mySession <- session()
myLibrary <- ssimLibrary(name = myLibraryName, session = mySession)
myProject <- project(myLibrary, project = "Definitions")

# Lists the folder location of SyncroSim Session
filepath(mySession)
#> [1] "C:\\PROGRA~1\\SYNCRO~1"

# Lists the version of SyncroSim Session
version(mySession)
#> [1] "3.1.20"

# Data frame of the packages installed with this version of SyncroSim
packages(mySession)
#>                     name version
#> 1             burnP3Plus   2.3.0
#> 2    burnP3PlusCell2Fire   2.2.0
#> 3    burnP3PlusFireSTARR   1.2.0
#> 4   burnP3PlusPrometheus   2.2.0
#> 5              demosales   2.1.0
#> 6                  dgsim   3.1.0
#> 7            ecoClassify   1.0.1
#> 8            ecoClassify   1.0.2
#> 9            ecoClassify   1.0.4
#> 10           ecoClassify   1.1.0
#> 11           ecoClassify   1.2.0
#> 12           ecoClassify   1.2.1
#> 13           ecoClassify   1.2.2
#> 14           ecoClassify   1.2.3
#> 15           ecoClassify   2.1.0
#> 16           ecoClassify   2.1.1
#> 17           ecoClassify   2.1.2
#> 18           ecoClassify   2.1.3
#> 19           ecoClassify   2.2.0
#> 20           ecoClassify   2.2.1
#> 21           ecoClassify   2.2.2
#> 22           ecoClassify   2.3.0
#> 23           ecoClassify   2.3.1
#> 24            helloworld   2.0.1
#> 25    helloworldPipeline   2.1.0
#> 26     helloworldSpatial   2.1.0
#> 27        helloworldTime   2.1.0
#> 28 helloworldUncertainty   2.1.0
#> 29          lucasbuilder   2.0.2
#> 30            prioritizr   2.2.1
#> 31            prioritizr   2.2.2
#> 32       resourceMonitor   1.0.0
#> 33                 stsim   4.0.1
#> 34                 stsim   4.3.5
#> 35                 stsim   4.3.8
#> 36                 stsim   4.5.0
#> 37                 stsim   4.5.4
#> 38                 wisdm  2.1.12
#>                                                                                                      description
#> 1                                                                                      Burn probability modeling
#> 2                                                                        Cell2Fire fire growth model for BurnP3+
#> 3                                                                        FireSTARR fire growth model for BurnP3+
#> 4                                                                       Prometheus fire growth model for BurnP3+
#> 5                                                                                   Demo Sales SyncroSim Package
#> 6                                                                 Simulates demographics of wildlife populations
#> 7                                                             Image classifier using semantic image segmentation
#> 8                                                             Image classifier using semantic image segmentation
#> 9                                                             Image classifier using semantic image segmentation
#> 10                                                            Image classifier using semantic image segmentation
#> 11                                                            Image classifier using semantic image segmentation
#> 12                                                            Image classifier using semantic image segmentation
#> 13                                                            Image classifier using semantic image segmentation
#> 14                                                            Image classifier using semantic image segmentation
#> 15                                                            Image classifier using semantic image segmentation
#> 16                                                            Image classifier using semantic image segmentation
#> 17                                                            Image classifier using semantic image segmentation
#> 18                                                            Image classifier using semantic image segmentation
#> 19                                                            Image classifier using semantic image segmentation
#> 20                                                            Image classifier using semantic image segmentation
#> 21                                                            Image classifier using semantic image segmentation
#> 22                                                            Image classifier using semantic image segmentation
#> 23                                                            Image classifier using semantic image segmentation
#> 24                                                                 Example demonstrating how to create a package
#> 25                                                    Example demonstrating how to use pipelines with an R model
#> 26                                                 Example demonstrating how to use spatial data with an R model
#> 27                                                    Example demonstrating how to use timesteps with an R model
#> 28                                                   Example demonstrating how to use iterations with an R model
#> 29 Integrates the Carbon Budget Model of the Canadian Forest Sector (CBM-CFS3) into the ST-Sim simulation model.
#> 30                                                                              Systematic conservation planning
#> 31                                                                              Systematic conservation planning
#> 32                                                            Monitors CPU and RAM usage during simulation runs.
#> 33                                                              The ST-Sim state-and-transition simulation model
#> 34                                                              The ST-Sim state-and-transition simulation model
#> 35                                                              The ST-Sim state-and-transition simulation model
#> 36                                                              The ST-Sim state-and-transition simulation model
#> 37                                                              The ST-Sim state-and-transition simulation model
#> 38                                                        Workbench for Integrated Species Distribution Modeling
#>                                                                                     location
#> 1             C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\burnP3Plus\\2.3.0
#> 2    C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\burnP3PlusCell2Fire\\2.2.0
#> 3    C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\burnP3PlusFireSTARR\\1.2.0
#> 4   C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\burnP3PlusPrometheus\\2.2.0
#> 5              C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\demosales\\2.1.0
#> 6                  C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\dgsim\\3.1.0
#> 7            C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\ecoClassify\\1.0.1
#> 8            C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\ecoClassify\\1.0.2
#> 9            C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\ecoClassify\\1.0.4
#> 10           C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\ecoClassify\\1.1.0
#> 11           C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\ecoClassify\\1.2.0
#> 12           C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\ecoClassify\\1.2.1
#> 13           C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\ecoClassify\\1.2.2
#> 14           C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\ecoClassify\\1.2.3
#> 15           C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\ecoClassify\\2.1.0
#> 16           C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\ecoClassify\\2.1.1
#> 17           C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\ecoClassify\\2.1.2
#> 18           C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\ecoClassify\\2.1.3
#> 19           C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\ecoClassify\\2.2.0
#> 20           C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\ecoClassify\\2.2.1
#> 21           C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\ecoClassify\\2.2.2
#> 22           C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\ecoClassify\\2.3.0
#> 23           C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\ecoClassify\\2.3.1
#> 24            C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\helloworld\\2.0.1
#> 25    C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\helloworldPipeline\\2.1.0
#> 26     C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\helloworldSpatial\\2.1.0
#> 27        C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\helloworldTime\\2.1.0
#> 28 C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\helloworldUncertainty\\2.1.0
#> 29          C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\lucasbuilder\\2.0.2
#> 30            C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\prioritizr\\2.2.1
#> 31            C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\prioritizr\\2.2.2
#> 32       C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\resourceMonitor\\1.0.0
#> 33                 C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\stsim\\4.0.1
#> 34                 C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\stsim\\4.3.5
#> 35                 C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\stsim\\4.3.8
#> 36                 C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\stsim\\4.5.0
#> 37                 C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\stsim\\4.5.4
#> 38                C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\wisdm\\2.1.12
#>    status
#> 1      OK
#> 2      OK
#> 3      OK
#> 4      OK
#> 5      OK
#> 6      OK
#> 7      OK
#> 8      OK
#> 9      OK
#> 10     OK
#> 11     OK
#> 12     OK
#> 13     OK
#> 14     OK
#> 15     OK
#> 16     OK
#> 17     OK
#> 18     OK
#> 19     OK
#> 20     OK
#> 21     OK
#> 22     OK
#> 23     OK
#> 24     OK
#> 25     OK
#> 26     OK
#> 27     OK
#> 28     OK
#> 29     OK
#> 30     OK
#> 31     OK
#> 32     OK
#> 33     OK
#> 34     OK
#> 35     OK
#> 36     OK
#> 37     OK
#> 38     OK

# Set a new SyncroSim Session for the SyncroSim Project
session(myProject) <- session(x = filepath(session(myProject)))
# }
```
