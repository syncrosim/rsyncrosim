# Create or return SyncroSim Session

Methods to create or return a SyncroSim
[`Session`](https://syncrosim.github.io/rsyncrosim/reference/Session-class.md).

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

  [`Project`](https://syncrosim.github.io/rsyncrosim/reference/Project-class.md)
  or
  [`Scenario`](https://syncrosim.github.io/rsyncrosim/reference/Scenario-class.md)
  object

- value:

  [`Session`](https://syncrosim.github.io/rsyncrosim/reference/Session-class.md)
  object

## Value

A SyncroSim
[`Session`](https://syncrosim.github.io/rsyncrosim/reference/Session-class.md)
object.

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
#> [1] "3.1.29"

# Data frame of the packages installed with this version of SyncroSim
packages(mySession)
#>                       name version
#> 1         burnP3FireHazard   2.0.0
#> 2               burnP3Plus   2.5.3
#> 3               burnP3Plus   2.3.0
#> 4               burnP3Plus   2.6.5
#> 5               burnP3Plus   2.6.7
#> 6      burnP3PlusCell2Fire   2.4.3
#> 7      burnP3PlusCell2Fire   2.2.0
#> 8      burnP3PlusFireSTARR   1.2.0
#> 9      burnP3PlusFireSTARR   1.5.5
#> 10     burnP3PlusFireSTARR   1.5.6
#> 11  burnP3PlusInputBuilder   1.0.0
#> 12    burnP3PlusPrometheus   2.2.0
#> 13    burnP3PlusPrometheus   2.5.4
#> 14             burnP3Sharp   1.0.0
#> 15                   dgsim   3.1.0
#> 16             ecoClassify   2.4.0
#> 17             ecoClassify   2.3.0
#> 18             ecoClassify   2.3.1
#> 19              helloworld   2.1.1
#> 20      helloworldPipeline   2.1.1
#> 21    helloworldPipelinePy   2.0.1
#> 22            helloworldPy   1.0.1
#> 23       helloworldSpatial   2.1.0
#> 24     helloworldSpatialPy   2.0.0
#> 25          helloworldTime   2.1.1
#> 26   helloworldUncertainty   2.1.1
#> 27 helloworldUncertaintyPy   2.0.1
#> 28       landfirevegmodels   2.0.0
#> 29           lotkavolterra   1.0.0
#> 30            lucasbuilder   2.0.3
#> 31            lucasbuilder   2.0.4
#> 32            lucasbuilder   2.0.2
#> 33               omniscape   2.3.0
#> 34               omniscape   2.2.0
#> 35              prioritizr   2.2.2
#> 36                   stsim   4.5.0
#> 37                   stsim   4.5.3
#> 38             stsimecodep   4.1.0
#> 39                   wisdm   2.5.0
#> 40                   wisdm   2.4.1
#> 41     wisdmStsimConnector   1.2.0
#>                                                                                                      description
#> 1                                                                                               burnP3FireHazard
#> 2                                                                                                     burnP3Plus
#> 3                                                                                      Burn probability modeling
#> 4                                                                                      Burn probability modeling
#> 5                                                                                      Burn probability modeling
#> 6                                                                                            burnP3PlusCell2Fire
#> 7                                                                        Cell2Fire fire growth model for BurnP3+
#> 8                                                                        FireSTARR fire growth model for BurnP3+
#> 9                                                                        FireSTARR fire growth model for BurnP3+
#> 10                                                                       FireSTARR fire growth model for BurnP3+
#> 11                                                                               Preprocess raw data for BurnP3+
#> 12                                                                      Prometheus fire growth model for BurnP3+
#> 13                                                                      Prometheus fire growth model for BurnP3+
#> 14                                                                                                   burnP3Sharp
#> 15                                                                Simulates demographics of wildlife populations
#> 16                                                                                                   ecoClassify
#> 17                                                            Image classifier using semantic image segmentation
#> 18                                                            Image classifier using semantic image segmentation
#> 19                                                                 Example demonstrating how to create a package
#> 20                                                    Example demonstrating how to use pipelines with an R model
#> 21                                                                                          helloworldPipelinePy
#> 22                                                                                                  helloworldPy
#> 23                                                 Example demonstrating how to use spatial data with an R model
#> 24                                                                                           helloworldSpatialPy
#> 25                                                    Example demonstrating how to use timesteps with an R model
#> 26                                                   Example demonstrating how to use iterations with an R model
#> 27                                                                                       helloworldUncertaintyPy
#> 28                                                                                    LANDFIRE vegetation models
#> 29                                                                       Predator-prey population dynamics model
#> 30                                                                                                  lucasbuilder
#> 31                                                                                                  lucasbuilder
#> 32 Integrates the Carbon Budget Model of the Canadian Forest Sector (CBM-CFS3) into the ST-Sim simulation model.
#> 33                                                                                                     omniscape
#> 34                                                 Omni-directional habitat connectivity based on circuit theory
#> 35                                                                              Systematic conservation planning
#> 36                                                              The ST-Sim state-and-transition simulation model
#> 37                                                              The ST-Sim state-and-transition simulation model
#> 38                             Calculates TNC's unified ecological departure from reference conditions in ST-Sim
#> 39                                                                                                         wisdm
#> 40                                                        Workbench for Integrated Species Distribution Modeling
#> 41                                                                    Connector package between WISDM and ST-Sim
#>                                                                                     location
#> 1                      C:\\Users\\VickiZhang\\Documents\\GH_ApexRMS\\BurnP3BasicSummary\\src
#> 2                              C:\\Users\\VickiZhang\\Documents\\GH_ApexRMS\\BurnP3Plus\\src
#> 3              C:\\Users\\VickiZhang\\AppData\\Local\\SyncroSim\\Packages\\burnP3Plus\\2.3.0
#> 4              C:\\Users\\VickiZhang\\AppData\\Local\\SyncroSim\\Packages\\burnP3Plus\\2.6.5
#> 5              C:\\Users\\VickiZhang\\AppData\\Local\\SyncroSim\\Packages\\burnP3Plus\\2.6.7
#> 6                     C:\\Users\\VickiZhang\\Documents\\GH_ApexRMS\\BurnP3PlusCell2Fire\\src
#> 7     C:\\Users\\VickiZhang\\AppData\\Local\\SyncroSim\\Packages\\burnP3PlusCell2Fire\\2.2.0
#> 8     C:\\Users\\VickiZhang\\AppData\\Local\\SyncroSim\\Packages\\burnP3PlusFireSTARR\\1.2.0
#> 9     C:\\Users\\VickiZhang\\AppData\\Local\\SyncroSim\\Packages\\burnP3PlusFireSTARR\\1.5.5
#> 10    C:\\Users\\VickiZhang\\AppData\\Local\\SyncroSim\\Packages\\burnP3PlusFireSTARR\\1.5.6
#> 11 C:\\Users\\VickiZhang\\AppData\\Local\\SyncroSim\\Packages\\burnP3PlusInputBuilder\\1.0.0
#> 12   C:\\Users\\VickiZhang\\AppData\\Local\\SyncroSim\\Packages\\burnP3PlusPrometheus\\2.2.0
#> 13   C:\\Users\\VickiZhang\\AppData\\Local\\SyncroSim\\Packages\\burnP3PlusPrometheus\\2.5.4
#> 14                            C:\\Users\\VickiZhang\\Documents\\GH_ApexRMS\\BurnP3Sharp\\src
#> 15                  C:\\Users\\VickiZhang\\AppData\\Local\\SyncroSim\\Packages\\dgsim\\3.1.0
#> 16                            C:\\Users\\VickiZhang\\Documents\\GH_ApexRMS\\ecoClassify\\src
#> 17            C:\\Users\\VickiZhang\\AppData\\Local\\SyncroSim\\Packages\\ecoClassify\\2.3.0
#> 18            C:\\Users\\VickiZhang\\AppData\\Local\\SyncroSim\\Packages\\ecoClassify\\2.3.1
#> 19             C:\\Users\\VickiZhang\\AppData\\Local\\SyncroSim\\Packages\\helloworld\\2.1.1
#> 20     C:\\Users\\VickiZhang\\AppData\\Local\\SyncroSim\\Packages\\helloworldPipeline\\2.1.1
#> 21                   C:\\Users\\VickiZhang\\Documents\\GH_ApexRMS\\helloworldPipelinePy\\src
#> 22                           C:\\Users\\VickiZhang\\Documents\\GH_ApexRMS\\helloworldPy\\src
#> 23      C:\\Users\\VickiZhang\\AppData\\Local\\SyncroSim\\Packages\\helloworldSpatial\\2.1.0
#> 24                    C:\\Users\\VickiZhang\\Documents\\GH_ApexRMS\\helloworldSpatialPy\\src
#> 25         C:\\Users\\VickiZhang\\AppData\\Local\\SyncroSim\\Packages\\helloworldTime\\2.1.1
#> 26  C:\\Users\\VickiZhang\\AppData\\Local\\SyncroSim\\Packages\\helloworldUncertainty\\2.1.1
#> 27                C:\\Users\\VickiZhang\\Documents\\GH_ApexRMS\\helloworldUncertaintyPy\\src
#> 28      C:\\Users\\VickiZhang\\AppData\\Local\\SyncroSim\\Packages\\landfirevegmodels\\2.0.0
#> 29          C:\\Users\\VickiZhang\\Documents\\SyncroSim\\My Packages\\LV-Package-Claude\\src
#> 30                           C:\\Users\\VickiZhang\\Documents\\GH_ApexRMS\\lucasbuilder\\src
#> 31                           C:\\Users\\VickiZhang\\Documents\\GH_ApexRMS\\lucasbuilder\\src
#> 32           C:\\Users\\VickiZhang\\AppData\\Local\\SyncroSim\\Packages\\lucasbuilder\\2.0.2
#> 33                              C:\\Users\\VickiZhang\\Documents\\GH_ApexRMS\\omniscape\\src
#> 34              C:\\Users\\VickiZhang\\AppData\\Local\\SyncroSim\\Packages\\omniscape\\2.2.0
#> 35             C:\\Users\\VickiZhang\\AppData\\Local\\SyncroSim\\Packages\\prioritizr\\2.2.2
#> 36                  C:\\Users\\VickiZhang\\AppData\\Local\\SyncroSim\\Packages\\stsim\\4.5.0
#> 37                  C:\\Users\\VickiZhang\\AppData\\Local\\SyncroSim\\Packages\\stsim\\4.5.3
#> 38            C:\\Users\\VickiZhang\\AppData\\Local\\SyncroSim\\Packages\\stsimecodep\\4.1.0
#> 39                                  C:\\Users\\VickiZhang\\Documents\\GH_ApexRMS\\wisdm\\src
#> 40                  C:\\Users\\VickiZhang\\AppData\\Local\\SyncroSim\\Packages\\wisdm\\2.4.1
#> 41    C:\\Users\\VickiZhang\\AppData\\Local\\SyncroSim\\Packages\\wisdmStsimConnector\\1.2.0
#>                                                                                                                                          status
#> 1       FAILED: Cannot load package. The package location is not valid: 'C:\\Users\\VickiZhang\\Documents\\GH_ApexRMS\\BurnP3BasicSummary\\src'
#> 2               FAILED: Cannot load package. The package location is not valid: 'C:\\Users\\VickiZhang\\Documents\\GH_ApexRMS\\BurnP3Plus\\src'
#> 3                                                                                                                                            OK
#> 4                                                                                                                                            OK
#> 5                                                                                                                                            OK
#> 6      FAILED: Cannot load package. The package location is not valid: 'C:\\Users\\VickiZhang\\Documents\\GH_ApexRMS\\BurnP3PlusCell2Fire\\src'
#> 7                                                                                                                                            OK
#> 8                                                                                                                                            OK
#> 9                                                                                                                                            OK
#> 10                                                                                                                                           OK
#> 11                                                                                                                                           OK
#> 12                                                                                                                                           OK
#> 13                                                                                                                                           OK
#> 14             FAILED: Cannot load package. The package location is not valid: 'C:\\Users\\VickiZhang\\Documents\\GH_ApexRMS\\BurnP3Sharp\\src'
#> 15                                                                                                                                           OK
#> 16             FAILED: Cannot load package. The package location is not valid: 'C:\\Users\\VickiZhang\\Documents\\GH_ApexRMS\\ecoClassify\\src'
#> 17                                                                                                                                           OK
#> 18                                                                                                                                           OK
#> 19                                                                                                                                           OK
#> 20                                                                                                                                           OK
#> 21    FAILED: Cannot load package. The package location is not valid: 'C:\\Users\\VickiZhang\\Documents\\GH_ApexRMS\\helloworldPipelinePy\\src'
#> 22            FAILED: Cannot load package. The package location is not valid: 'C:\\Users\\VickiZhang\\Documents\\GH_ApexRMS\\helloworldPy\\src'
#> 23                                                                                                                                           OK
#> 24     FAILED: Cannot load package. The package location is not valid: 'C:\\Users\\VickiZhang\\Documents\\GH_ApexRMS\\helloworldSpatialPy\\src'
#> 25                                                                                                                                           OK
#> 26                                                                                                                                           OK
#> 27 FAILED: Cannot load package. The package location is not valid: 'C:\\Users\\VickiZhang\\Documents\\GH_ApexRMS\\helloworldUncertaintyPy\\src'
#> 28                                                                                                                                           OK
#> 29                                                                                                                                           OK
#> 30            FAILED: Cannot load package. The package location is not valid: 'C:\\Users\\VickiZhang\\Documents\\GH_ApexRMS\\lucasbuilder\\src'
#> 31            FAILED: Cannot load package. The package location is not valid: 'C:\\Users\\VickiZhang\\Documents\\GH_ApexRMS\\lucasbuilder\\src'
#> 32                                                                                                                                           OK
#> 33               FAILED: Cannot load package. The package location is not valid: 'C:\\Users\\VickiZhang\\Documents\\GH_ApexRMS\\omniscape\\src'
#> 34                                                                                                                                           OK
#> 35                                                                                                                                           OK
#> 36                                                                                                                                           OK
#> 37                                                                                                                                           OK
#> 38                                                                                                                                           OK
#> 39                   FAILED: Cannot load package. The package location is not valid: 'C:\\Users\\VickiZhang\\Documents\\GH_ApexRMS\\wisdm\\src'
#> 40                                                                                                                                           OK
#> 41                                                                                                                                           OK

# Set a new SyncroSim Session for the SyncroSim Project
session(myProject) <- session(x = filepath(session(myProject)))
# }
```
