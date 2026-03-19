# Installed or available packages

Retrieves the packages installed or available in the current session if
called on a
[`Session`](https://syncrosim.github.io/rsyncrosim/reference/Session-class.md)
object, or the packages added to a SyncroSim Library if called on a
[`SsimLibrary`](https://syncrosim.github.io/rsyncrosim/reference/SsimLibrary-class.md)
object.

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

  [`Session`](https://syncrosim.github.io/rsyncrosim/reference/Session-class.md)
  or
  [`SsimLibrary`](https://syncrosim.github.io/rsyncrosim/reference/SsimLibrary-class.md)
  object. If `NULL` (default),
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
#> 13           ecoClassify   2.3.2
#> 14            helloworld   2.1.0
#> 15            helloworld   2.1.1
#> 16    helloworldPipeline   2.1.0
#> 17    helloworldPipeline   2.1.1
#> 18     helloworldSpatial   2.1.0
#> 19        helloworldTime   2.1.0
#> 20        helloworldTime   2.1.1
#> 21 helloworldUncertainty   2.1.0
#> 22 helloworldUncertainty   2.1.1
#> 23     landfirevegmodels   2.0.0
#> 24          lucasbuilder   2.0.2
#> 25          lucasbuilder   2.0.4
#> 26             omniscape   2.2.0
#> 27            prioritizr   2.2.2
#> 28            prioritizr   3.0.0
#> 29                 stsim   4.3.8
#> 30                 stsim   4.5.0
#> 31                 stsim   4.5.1
#> 32                 stsim   4.5.2
#> 33                 stsim   4.5.3
#> 34           stsimecodep   4.1.0
#> 35                 wisdm   2.2.0
#> 36                 wisdm   2.3.0
#> 37                 wisdm   2.4.1
#> 38   wisdmStsimConnector   1.2.0
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
#> 13                                                                  ecoClassify package for image classification
#> 14                                                       Example demonstrating how to create a SyncroSim package
#> 15                                                       Example demonstrating how to create a SyncroSim package
#> 16                                                    Example demonstrating how to use pipelines with an R model
#> 17                                                    Example demonstrating how to use pipelines with an R model
#> 18                                                 Example demonstrating how to use spatial data with an R model
#> 19                                                    Example demonstrating how to use timesteps with an R model
#> 20                                                    Example demonstrating how to use timesteps with an R model
#> 21                                                   Example demonstrating how to use iterations with an R model
#> 22                                                   Example demonstrating how to use iterations with an R model
#> 23                                                                                    LANDFIRE vegetation models
#> 24 Integrates the Carbon Budget Model of the Canadian Forest Sector (CBM-CFS3) into the ST-Sim simulation model.
#> 25 Integrates the Carbon Budget Model of the Canadian Forest Sector (CBM-CFS3) into the ST-Sim simulation model.
#> 26                                                 Omni-directional habitat connectivity based on circuit theory
#> 27                                                                              Systematic conservation planning
#> 28                                                                              Systematic conservation planning
#> 29                                                              The ST-Sim state-and-transition simulation model
#> 30                                                              The ST-Sim state-and-transition simulation model
#> 31                                                              The ST-Sim state-and-transition simulation model
#> 32                                                              The ST-Sim state-and-transition simulation model
#> 33                                                              The ST-Sim state-and-transition simulation model
#> 34                             Calculates TNC's unified ecological departure from reference conditions in ST-Sim
#> 35                                                        Workbench for Integrated Species Distribution Modeling
#> 36                                                        Workbench for Integrated Species Distribution Modeling
#> 37                                                        Workbench for Integrated Species Distribution Modeling
#> 38                                                                    Connector package between WISDM and ST-Sim
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
#> 13        https://apexrms.github.io/ecoClassify/
#> 14         https://apexrms.github.io/helloworld/
#> 15         https://apexrms.github.io/helloworld/
#> 16 https://apexrms.github.io/helloworldEnhanced/
#> 17 https://apexrms.github.io/helloworldEnhanced/
#> 18 https://apexrms.github.io/helloworldEnhanced/
#> 19 https://apexrms.github.io/helloworldEnhanced/
#> 20 https://apexrms.github.io/helloworldEnhanced/
#> 21 https://apexrms.github.io/helloworldEnhanced/
#> 22 https://apexrms.github.io/helloworldEnhanced/
#> 23  https://apexrms.github.io/landfirevegmodels/
#> 24       https://apexrms.github.io/lucasbuilder/
#> 25       https://apexrms.github.io/lucasbuilder/
#> 26          https://apexrms.github.io/omniscape/
#> 27          https://apexrms.github.io/prioritizr
#> 28          https://apexrms.github.io/prioritizr
#> 29                       https://docs.stsim.net/
#> 30                       https://docs.stsim.net/
#> 31                       https://docs.stsim.net/
#> 32                       https://docs.stsim.net/
#> 33                       https://docs.stsim.net/
#> 34                       https://docs.stsim.net/
#> 35              https://apexrms.github.io/wisdm/
#> 36              https://apexrms.github.io/wisdm/
#> 37              https://apexrms.github.io/wisdm/
#> 38              https://apexrms.github.io/wisdm/
 
# Check the package(s) in your SsimLibrary
packages(myLibrary)
#>    name                                      description version schema status
#> 1  core                           SyncroSim Core Package  3.1.29    3.1     OK
#> 2 stsim The ST-Sim state-and-transition simulation model   4.5.3    4.5     OK
# }
```
