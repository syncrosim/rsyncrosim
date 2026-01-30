# Installed or available packages

Retrieves the packages installed or available in the current session if
called on a `Session` object, or the packages added to a SyncroSim
Library if called on a `SsimLibrary` object.

## Usage

``` r
packages(ssimObject = NULL, installed = TRUE)

# S4 method for class 'character'
packages(ssimObject = NULL, installed = TRUE)

# S4 method for class 'missingOrNULL'
packages(ssimObject = NULL, installed = TRUE)

# S4 method for class 'Session'
packages(ssimObject = NULL, installed = TRUE)

# S4 method for class 'SsimLibrary'
packages(ssimObject)
```

## Arguments

- ssimObject:

  `Session` or `SsimLibrary` object. If `NULL` (default),
  [`session()`](https://syncrosim.github.io/rsyncrosim/reference/session.md)
  will be used

- installed:

  logical or character. `TRUE` (default) to list installed packages or
  `FALSE` to list available packages on the server

## Value

Returns a `data.frame` of packages installed or templates available for
a specified package.

## Examples

``` r
# \donttest{
# Set the file path and name of the new SsimLibrary
myLibraryName <- file.path(tempdir(),"testlib")

# Set the SyncroSim Session and SsimLibrary
mySession <- session()
myLibrary <- ssimLibrary(name = myLibraryName, session = mySession)

# List all installed packages
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
#> 28            helloworld   2.0.1
#> 29    helloworldPipeline   2.1.1
#> 30     helloworldSpatial   2.1.0
#> 31        helloworldTime   2.1.1
#> 32 helloworldUncertainty   2.1.1
#> 33          lucasbuilder   2.0.2
#> 34          lucasbuilder   2.0.4
#> 35             omniscape   2.2.0
#> 36             omniscape   2.3.0
#> 37             omniscape   2.6.0
#> 38            prioritizr   2.2.1
#> 39            prioritizr   2.2.2
#> 40       resourceMonitor   1.0.0
#> 41                 stsim   4.3.8
#> 42                 stsim   4.5.0
#> 43                 stsim   4.5.2
#> 44                 stsim   4.5.3
#> 45                 wisdm  2.1.12
#> 46                 wisdm   2.3.0
#> 47                 wisdm   2.4.1
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
#> 28                                                                 Example demonstrating how to create a package
#> 29                                                    Example demonstrating how to use pipelines with an R model
#> 30                                                 Example demonstrating how to use spatial data with an R model
#> 31                                                    Example demonstrating how to use timesteps with an R model
#> 32                                                   Example demonstrating how to use iterations with an R model
#> 33 Integrates the Carbon Budget Model of the Canadian Forest Sector (CBM-CFS3) into the ST-Sim simulation model.
#> 34 Integrates the Carbon Budget Model of the Canadian Forest Sector (CBM-CFS3) into the ST-Sim simulation model.
#> 35                                                 Omni-directional habitat connectivity based on circuit theory
#> 36                                                 Omni-directional habitat connectivity based on circuit theory
#> 37                                                 Omni-directional habitat connectivity based on circuit theory
#> 38                                                                              Systematic conservation planning
#> 39                                                                              Systematic conservation planning
#> 40                                                            Monitors CPU and RAM usage during simulation runs.
#> 41                                                              The ST-Sim state-and-transition simulation model
#> 42                                                              The ST-Sim state-and-transition simulation model
#> 43                                                              The ST-Sim state-and-transition simulation model
#> 44                                                              The ST-Sim state-and-transition simulation model
#> 45                                                        Workbench for Integrated Species Distribution Modeling
#> 46                                                        Workbench for Integrated Species Distribution Modeling
#> 47                                                        Workbench for Integrated Species Distribution Modeling
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
#> 28            C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\helloworld\\2.0.1
#> 29    C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\helloworldPipeline\\2.1.1
#> 30     C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\helloworldSpatial\\2.1.0
#> 31        C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\helloworldTime\\2.1.1
#> 32 C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\helloworldUncertainty\\2.1.1
#> 33          C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\lucasbuilder\\2.0.2
#> 34          C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\lucasbuilder\\2.0.4
#> 35             C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\omniscape\\2.2.0
#> 36             C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\omniscape\\2.3.0
#> 37             C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\omniscape\\2.6.0
#> 38            C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\prioritizr\\2.2.1
#> 39            C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\prioritizr\\2.2.2
#> 40       C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\resourceMonitor\\1.0.0
#> 41                 C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\stsim\\4.3.8
#> 42                 C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\stsim\\4.5.0
#> 43                 C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\stsim\\4.5.2
#> 44                 C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\stsim\\4.5.3
#> 45                C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\wisdm\\2.1.12
#> 46                 C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\wisdm\\2.3.0
#> 47                 C:\\Users\\HannahAdams\\AppData\\Local\\SyncroSim\\Packages\\wisdm\\2.4.1
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

# List all available packages on the server (including currently installed)
packages(installed = FALSE)
#>                     name version
#> 1             burnP3Plus   2.3.0
#> 2    burnP3PlusCell2Fire   2.2.0
#> 3    burnP3PlusFireSTARR   1.2.0
#> 4   burnP3PlusPrometheus   2.2.0
#> 5              demosales   2.1.0
#> 6                  dgsim   3.1.0
#> 7            ecoClassify   1.1.0
#> 8            ecoClassify   1.2.1
#> 9            ecoClassify   2.1.2
#> 10           ecoClassify   2.2.0
#> 11           ecoClassify   2.3.0
#> 12           ecoClassify   2.3.1
#> 13            helloworld   2.1.0
#> 14            helloworld   2.1.1
#> 15    helloworldPipeline   2.1.0
#> 16    helloworldPipeline   2.1.1
#> 17     helloworldSpatial   2.1.0
#> 18        helloworldTime   2.1.0
#> 19        helloworldTime   2.1.1
#> 20 helloworldUncertainty   2.1.0
#> 21 helloworldUncertainty   2.1.1
#> 22     landfirevegmodels   2.0.0
#> 23          lucasbuilder   2.0.2
#> 24          lucasbuilder   2.0.4
#> 25             omniscape   2.2.0
#> 26            prioritizr   2.2.2
#> 27            prioritizr   3.0.0
#> 28                 stsim   4.3.8
#> 29                 stsim   4.5.0
#> 30                 stsim   4.5.1
#> 31                 stsim   4.5.2
#> 32                 stsim   4.5.3
#> 33           stsimecodep   4.1.0
#> 34                 wisdm   2.2.0
#> 35                 wisdm   2.3.0
#> 36                 wisdm   2.4.1
#> 37   wisdmStsimConnector   1.2.0
#>                                                                                                      description
#> 1                                                                 Burn-P3+ package for burn probability modeling
#> 2                                                                Cell2Fire fire growth model package for BurnP3+
#> 3                                                                FireSTARR fire growth model package for BurnP3+
#> 4                                                               Prometheus fire growth model package for BurnP3+
#> 5                                                                                   Demo Sales SyncroSim Package
#> 6                                                                 Simulates demographics of wildlife populations
#> 7                                                                   ecoClassify package for image classification
#> 8                                                                   ecoClassify package for image classification
#> 9                                                                   ecoClassify package for image classification
#> 10                                                                  ecoClassify package for image classification
#> 11                                                                  ecoClassify package for image classification
#> 12                                                                  ecoClassify package for image classification
#> 13                                                       Example demonstrating how to create a SyncroSim package
#> 14                                                       Example demonstrating how to create a SyncroSim package
#> 15                                                    Example demonstrating how to use pipelines with an R model
#> 16                                                    Example demonstrating how to use pipelines with an R model
#> 17                                                 Example demonstrating how to use spatial data with an R model
#> 18                                                    Example demonstrating how to use timesteps with an R model
#> 19                                                    Example demonstrating how to use timesteps with an R model
#> 20                                                   Example demonstrating how to use iterations with an R model
#> 21                                                   Example demonstrating how to use iterations with an R model
#> 22                                                                                    LANDFIRE vegetation models
#> 23 Integrates the Carbon Budget Model of the Canadian Forest Sector (CBM-CFS3) into the ST-Sim simulation model.
#> 24 Integrates the Carbon Budget Model of the Canadian Forest Sector (CBM-CFS3) into the ST-Sim simulation model.
#> 25                                                 Omni-directional habitat connectivity based on circuit theory
#> 26                                                                              Systematic conservation planning
#> 27                                                                              Systematic conservation planning
#> 28                                                              The ST-Sim state-and-transition simulation model
#> 29                                                              The ST-Sim state-and-transition simulation model
#> 30                                                              The ST-Sim state-and-transition simulation model
#> 31                                                              The ST-Sim state-and-transition simulation model
#> 32                                                              The ST-Sim state-and-transition simulation model
#> 33                             Calculates TNC's unified ecological departure from reference conditions in ST-Sim
#> 34                                                        Workbench for Integrated Species Distribution Modeling
#> 35                                                        Workbench for Integrated Species Distribution Modeling
#> 36                                                        Workbench for Integrated Species Distribution Modeling
#> 37                                                                    Connector package between WISDM and ST-Sim
#>                                              url
#> 1           https://burnp3.github.io/BurnP3Plus/
#> 2           https://burnp3.github.io/BurnP3Plus/
#> 3           https://burnp3.github.io/BurnP3Plus/
#> 4           https://burnp3.github.io/BurnP3Plus/
#> 5           https://apexrms.github.io/demosales/
#> 6               https://apexrms.github.io/dgsim/
#> 7         https://apexrms.github.io/ecoClassify/
#> 8         https://apexrms.github.io/ecoClassify/
#> 9         https://apexrms.github.io/ecoClassify/
#> 10        https://apexrms.github.io/ecoClassify/
#> 11        https://apexrms.github.io/ecoClassify/
#> 12        https://apexrms.github.io/ecoClassify/
#> 13         https://apexrms.github.io/helloworld/
#> 14         https://apexrms.github.io/helloworld/
#> 15 https://apexrms.github.io/helloworldEnhanced/
#> 16 https://apexrms.github.io/helloworldEnhanced/
#> 17 https://apexrms.github.io/helloworldEnhanced/
#> 18 https://apexrms.github.io/helloworldEnhanced/
#> 19 https://apexrms.github.io/helloworldEnhanced/
#> 20 https://apexrms.github.io/helloworldEnhanced/
#> 21 https://apexrms.github.io/helloworldEnhanced/
#> 22  https://apexrms.github.io/landfirevegmodels/
#> 23       https://apexrms.github.io/lucasbuilder/
#> 24       https://apexrms.github.io/lucasbuilder/
#> 25          https://apexrms.github.io/omniscape/
#> 26          https://apexrms.github.io/prioritizr
#> 27          https://apexrms.github.io/prioritizr
#> 28                       https://docs.stsim.net/
#> 29                       https://docs.stsim.net/
#> 30                       https://docs.stsim.net/
#> 31                       https://docs.stsim.net/
#> 32                       https://docs.stsim.net/
#> 33                       https://docs.stsim.net/
#> 34              https://apexrms.github.io/wisdm/
#> 35              https://apexrms.github.io/wisdm/
#> 36              https://apexrms.github.io/wisdm/
#> 37              https://apexrms.github.io/wisdm/
 
# Check the package(s) in your SsimLibrary
packages(myLibrary)
#>    name                                      description version schema status
#> 1  core                           SyncroSim Core Package  3.1.24    3.1     OK
#> 2 stsim The ST-Sim state-and-transition simulation model   4.5.3    4.5     OK
# }
```
