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
#> [1] "C:\\PROGRA~1\\SYNCRO~3"

# Lists the version of SyncroSim Session
version(mySession)
#> [1] "3.1.30"

# Data frame of the packages installed with this version of SyncroSim
packages(mySession)
#>                     name version
#> 1             burnP3Plus   2.3.0
#> 2             burnP3Plus   2.6.5
#> 3    burnP3PlusCell2Fire   2.2.0
#> 4    burnP3PlusFireSTARR   1.2.0
#> 5    burnP3PlusFireSTARR   1.5.3
#> 6    burnP3PlusFireSTARR   1.5.5
#> 7   burnP3PlusPrometheus   2.2.0
#> 8              demosales   2.1.0
#> 9                  dgsim   3.1.0
#> 10           ecoClassify   1.0.1
#> 11           ecoClassify   1.0.2
#> 12           ecoClassify   1.0.4
#> 13           ecoClassify   1.1.0
#> 14           ecoClassify   1.2.0
#> 15           ecoClassify   1.2.1
#> 16           ecoClassify   1.2.2
#> 17           ecoClassify   1.2.3
#> 18           ecoClassify   2.1.0
#> 19           ecoClassify   2.1.1
#> 20           ecoClassify   2.1.2
#> 21           ecoClassify   2.1.3
#> 22           ecoClassify   2.2.0
#> 23           ecoClassify   2.2.1
#> 24           ecoClassify   2.2.2
#> 25           ecoClassify   2.3.0
#> 26           ecoClassify   2.3.1
#> 27           ecoClassify   2.3.2
#> 28           ecoClassify   2.4.0
#> 29            helloworld   2.0.1
#> 30    helloworldPipeline   2.1.1
#> 31     helloworldSpatial   2.1.0
#> 32        helloworldTime   2.1.1
#> 33 helloworldUncertainty   2.1.1
#> 34          lucasbuilder   2.0.2
#> 35          lucasbuilder   2.0.4
#> 36             omniscape   2.2.0
#> 37             omniscape   2.3.0
#> 38             omniscape   2.6.0
#> 39            prioritizr   2.2.1
#> 40            prioritizr   2.2.2
#> 41            prioritizr   3.0.0
#> 42       resourceMonitor   1.0.0
#> 43         specIndexView   1.0.0
#> 44                 stsim   4.1.2
#> 45                 stsim   4.3.5
#> 46                 stsim   4.3.8
#> 47                 stsim   4.5.0
#> 48                 stsim   4.5.3
#> 49                 wisdm  2.1.12
#> 50                 wisdm   2.3.0
#> 51                 wisdm   2.4.1
#>                                                                                                      description
#> 1                                                                                      Burn probability modeling
#> 2                                                                                      Burn probability modeling
#> 3                                                                        Cell2Fire fire growth model for BurnP3+
#> 4                                                                        FireSTARR fire growth model for BurnP3+
#> 5                                                                        FireSTARR fire growth model for BurnP3+
#> 6                                                                        FireSTARR fire growth model for BurnP3+
#> 7                                                                       Prometheus fire growth model for BurnP3+
#> 8                                                                                   Demo Sales SyncroSim Package
#> 9                                                                 Simulates demographics of wildlife populations
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
#> 24                                                            Image classifier using semantic image segmentation
#> 25                                                            Image classifier using semantic image segmentation
#> 26                                                            Image classifier using semantic image segmentation
#> 27                                                            Image classifier using semantic image segmentation
#> 28                                                            Image classifier using semantic image segmentation
#> 29                                                                 Example demonstrating how to create a package
#> 30                                                    Example demonstrating how to use pipelines with an R model
#> 31                                                 Example demonstrating how to use spatial data with an R model
#> 32                                                    Example demonstrating how to use timesteps with an R model
#> 33                                                   Example demonstrating how to use iterations with an R model
#> 34 Integrates the Carbon Budget Model of the Canadian Forest Sector (CBM-CFS3) into the ST-Sim simulation model.
#> 35 Integrates the Carbon Budget Model of the Canadian Forest Sector (CBM-CFS3) into the ST-Sim simulation model.
#> 36                                                 Omni-directional habitat connectivity based on circuit theory
#> 37                                                 Omni-directional habitat connectivity based on circuit theory
#> 38                                                 Omni-directional habitat connectivity based on circuit theory
#> 39                                                                              Systematic conservation planning
#> 40                                                                              Systematic conservation planning
#> 41                                                                              Systematic conservation planning
#> 42                                                            Monitors CPU and RAM usage during simulation runs.
#> 43                              Extracts and displays NDVI and NDWI spectral index bands from multi-band rasters
#> 44                                                              The ST-Sim state-and-transition simulation model
#> 45                                                              The ST-Sim state-and-transition simulation model
#> 46                                                              The ST-Sim state-and-transition simulation model
#> 47                                                              The ST-Sim state-and-transition simulation model
#> 48                                                              The ST-Sim state-and-transition simulation model
#> 49                                                        Workbench for Integrated Species Distribution Modeling
#> 50                                                        Workbench for Integrated Species Distribution Modeling
#> 51                                                        Workbench for Integrated Species Distribution Modeling
#>                                                                                     location
#> 1             C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\burnP3Plus\\2.3.0
#> 2             C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\burnP3Plus\\2.6.5
#> 3    C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\burnP3PlusCell2Fire\\2.2.0
#> 4    C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\burnP3PlusFireSTARR\\1.2.0
#> 5    C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\burnP3PlusFireSTARR\\1.5.3
#> 6    C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\burnP3PlusFireSTARR\\1.5.5
#> 7   C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\burnP3PlusPrometheus\\2.2.0
#> 8              C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\demosales\\2.1.0
#> 9                  C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\dgsim\\3.1.0
#> 10           C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\ecoClassify\\1.0.1
#> 11           C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\ecoClassify\\1.0.2
#> 12           C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\ecoClassify\\1.0.4
#> 13           C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\ecoClassify\\1.1.0
#> 14           C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\ecoClassify\\1.2.0
#> 15           C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\ecoClassify\\1.2.1
#> 16           C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\ecoClassify\\1.2.2
#> 17           C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\ecoClassify\\1.2.3
#> 18           C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\ecoClassify\\2.1.0
#> 19           C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\ecoClassify\\2.1.1
#> 20           C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\ecoClassify\\2.1.2
#> 21           C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\ecoClassify\\2.1.3
#> 22           C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\ecoClassify\\2.2.0
#> 23           C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\ecoClassify\\2.2.1
#> 24           C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\ecoClassify\\2.2.2
#> 25           C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\ecoClassify\\2.3.0
#> 26           C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\ecoClassify\\2.3.1
#> 27           C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\ecoClassify\\2.3.2
#> 28           C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\ecoClassify\\2.4.0
#> 29            C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\helloworld\\2.0.1
#> 30    C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\helloworldPipeline\\2.1.1
#> 31     C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\helloworldSpatial\\2.1.0
#> 32        C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\helloworldTime\\2.1.1
#> 33 C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\helloworldUncertainty\\2.1.1
#> 34          C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\lucasbuilder\\2.0.2
#> 35          C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\lucasbuilder\\2.0.4
#> 36             C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\omniscape\\2.2.0
#> 37             C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\omniscape\\2.3.0
#> 38             C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\omniscape\\2.6.0
#> 39            C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\prioritizr\\2.2.1
#> 40            C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\prioritizr\\2.2.2
#> 41            C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\prioritizr\\3.0.0
#> 42       C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\resourceMonitor\\1.0.0
#> 43    C:\\Users\\HannahAdams\\Documents\\GitHub\\A363-McMurrayMetisBeaver\\ssim-package\\src
#> 44                 C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\stsim\\4.1.2
#> 45                 C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\stsim\\4.3.5
#> 46                 C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\stsim\\4.3.8
#> 47                 C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\stsim\\4.5.0
#> 48                 C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\stsim\\4.5.3
#> 49                C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\wisdm\\2.1.12
#> 50                 C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\wisdm\\2.3.0
#> 51                 C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\wisdm\\2.4.1
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
#> 39     OK
#> 40     OK
#> 41     OK
#> 42     OK
#> 43     OK
#> 44     OK
#> 45     OK
#> 46     OK
#> 47     OK
#> 48     OK
#> 49     OK
#> 50     OK
#> 51     OK

# Set a new SyncroSim Session for the SyncroSim Project
session(myProject) <- session(x = filepath(session(myProject)))
# }
```
