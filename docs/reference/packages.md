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
#> 14    helloworldPipeline   2.1.0
#> 15     helloworldSpatial   2.1.0
#> 16        helloworldTime   2.1.0
#> 17 helloworldUncertainty   2.1.0
#> 18          lucasbuilder   2.0.2
#> 19             omniscape   2.2.0
#> 20            prioritizr   2.2.2
#> 21            prioritizr   3.0.0
#> 22                 stsim   4.3.8
#> 23                 stsim   4.5.0
#> 24                 stsim   4.5.1
#> 25                 stsim   4.5.2
#> 26                 stsim   4.5.3
#> 27           stsimecodep   4.1.0
#> 28                 wisdm   2.2.0
#> 29                 wisdm   2.3.0
#> 30                 wisdm   2.4.1
#> 31   wisdmStsimConnector   1.2.0
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
#> 14                                                    Example demonstrating how to use pipelines with an R model
#> 15                                                 Example demonstrating how to use spatial data with an R model
#> 16                                                    Example demonstrating how to use timesteps with an R model
#> 17                                                   Example demonstrating how to use iterations with an R model
#> 18 Integrates the Carbon Budget Model of the Canadian Forest Sector (CBM-CFS3) into the ST-Sim simulation model.
#> 19                                                 Omni-directional habitat connectivity based on circuit theory
#> 20                                                                              Systematic conservation planning
#> 21                                                                              Systematic conservation planning
#> 22                                                              The ST-Sim state-and-transition simulation model
#> 23                                                              The ST-Sim state-and-transition simulation model
#> 24                                                              The ST-Sim state-and-transition simulation model
#> 25                                                              The ST-Sim state-and-transition simulation model
#> 26                                                              The ST-Sim state-and-transition simulation model
#> 27                             Calculates TNC's unified ecological departure from reference conditions in ST-Sim
#> 28                                                        Workbench for Integrated Species Distribution Modeling
#> 29                                                        Workbench for Integrated Species Distribution Modeling
#> 30                                                        Workbench for Integrated Species Distribution Modeling
#> 31                                                                    Connector package between WISDM and ST-Sim
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
#> 14 https://apexrms.github.io/helloworldEnhanced/
#> 15 https://apexrms.github.io/helloworldEnhanced/
#> 16 https://apexrms.github.io/helloworldEnhanced/
#> 17 https://apexrms.github.io/helloworldEnhanced/
#> 18                                           N/A
#> 19          https://apexrms.github.io/omniscape/
#> 20          https://apexrms.github.io/prioritizr
#> 21          https://apexrms.github.io/prioritizr
#> 22                       https://docs.stsim.net/
#> 23                       https://docs.stsim.net/
#> 24                       https://docs.stsim.net/
#> 25                       https://docs.stsim.net/
#> 26                       https://docs.stsim.net/
#> 27                       https://docs.stsim.net/
#> 28              https://apexrms.github.io/wisdm/
#> 29              https://apexrms.github.io/wisdm/
#> 30              https://apexrms.github.io/wisdm/
#> 31              https://apexrms.github.io/wisdm/
 
# Check the package(s) in your SsimLibrary
packages(myLibrary)
#>    name                                      description version schema status
#> 1  core                           SyncroSim Core Package  3.1.20    3.1     OK
#> 2 stsim The ST-Sim state-and-transition simulation model   4.5.4    4.5     OK
# }
```
