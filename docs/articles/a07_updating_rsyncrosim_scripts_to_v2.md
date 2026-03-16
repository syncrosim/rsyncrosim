# Updating scripts to rsyncrosim v2

The introduction of [SyncroSim Studio](https://syncrosim.com/studio/)
brought some structural changes and new features, which are reflected in
the `rsyncrosim` package. Use the guide below to update your
`rsyncrosim v1` scripts to be compatible with `rsyncrosim v2`.

**Note:** [SyncroSim v3.0.9](https://syncrosim.com/studio-download/) or
higher is required to use `rsyncrosim v2.0.0` or higher.

## Updated functions

### Installing/uninstalling packages

Previously, the `addPackage` and `removePackage` functions were used to
install and uninstall packages from SyncroSim. Now to install and
uninstall packages from SyncroSim, use:

``` r
# Installs the selected package(s) to SyncroSim
installPackage()  

# Uninstalls the selected package(s) from SyncroSim
uninstallPackage() 
```

You can also have multiple versions of a package installed. Use the
`versions` argument in the
[`installPackage()`](https://syncrosim.github.io/rsyncrosim/reference/installPackage.md)
function to specify which version of the package you would like to
install. If you do not specify a version, then the latest version of the
package will be installed.

``` r
# Install ST-Sim version 4.0.0
installPackage(myLibrary, packages = "stsim", versions = "4.0.0")

# Install the latest version of ST-Sim on the package server
installPackage(myLibrary, packages = "stsim")
```

You can also use the `versions` argument in the
[`uninstallPackage()`](https://syncrosim.github.io/rsyncrosim/reference/uninstallPackage.md)
function to specify which version you would like to uninstall. If you do
not specify a version, then all versions of that package will be
uninstalled.

``` r
# Uninstall ST-Sim version 4.0.0
uninstallPackage(myLibrary, packages = "stsim", versions = "4.0.0")

# Uninstall all installed versions of ST-Sim
uninstallPackage(myLibrary, packages = "stsim")
```

### Adding/removing packages

Now you can use multiple SyncroSim packages in a single library. To add
and remove packages from your library, use:

``` r
# Adds package(s) to the library
addPackage()    

# Removes package(s) from the library
removePackage() 
```

**Note:** when you remove a package from your library any associated
datasheets will be removed as well.

You can also choose which package version to use within a library. Use
the `versions` argument in the
[`addPackage()`](https://syncrosim.github.io/rsyncrosim/reference/addPackage.md)
function to load a specific version of a package in a library, or to
change the version of a package that the library uses.

``` r
# Add ST-Sim version 4.0.0 to your library
addPackage(myLibrary, packages = "stsim", versions = "4.0.0")

# Update the version of ST-Sim that your library uses
addPackage(myLibrary, packages = "stsim", versions = "4.0.1")
```

You do not need to specify a version when removing a package using
[`removePackage()`](https://syncrosim.github.io/rsyncrosim/reference/removePackage.md)
because only one version of a package can be loaded in a library at a
time.

### Creating libraries

Now that addon packages no longer exist and SyncroSim libraries can
support multiple packages, the following changes have been made to the
[`ssimLibrary()`](https://syncrosim.github.io/rsyncrosim/reference/ssimLibrary.md)
function:

- The `addon` argument has been removed.
- The `template` argument has been removed. This is a temporary change,
  but in the meantime you can download template libraries from
  [SyncroSim Cloud](https://cloud.syncrosim.com/).
- The `package` argument has been renamed to `packages` (plural).

``` r
# rsyncrosim version 1:
myLibrary <- ssimLibrary(package = "PackageName", addon = "AddonName")

# rsyncrosim version 2:
myLibrary <- ssimLibrary(packages = c("stsim", "helloworldSpatial"))
```

### Library information

Since multiple packages can be loaded in a library, the
`info(ssimLibrary)` no longer returns the following library attributes:

- *Package name*
- *Current package version*
- *Minimum package version*

In addition, now that you can link models from various packages in a
pipeline, and outputs from one transformer can be inputs to the next
transformer in a pipeline, the *Input* and *Output* folders have been
combined into a single *Data* folder. See the [Introduction to
rsyncrosim](https://syncrosim.github.io/rsyncrosim/articles/a01_rsyncrosim_vignette_basic.html#access-model-metadata)
for details.

### Dependencies

Scenario dependencies are now set using the `<-` operator in
`rsyncrosim`. See below for examples on how the usage of the
[`dependency()`](https://syncrosim.github.io/rsyncrosim/reference/dependency.md)
function has changed.

To view the existing dependencies for a scenario:

``` r
dependency(myScenario)
```

    ##   ScenarioId       Name Priority
    ## 1          1 Scenario 1        1

To set dependencies for a scenario:

``` r
# rsyncrosim version 1:
dependency(myScenario, dependency = c("Scenario 2", "Scenario 3"))
```

``` r
# rsyncrosim version 2:
dependency(myScenario) <- c("Scenario 2", "Scenario 3")

dependency(myScenario)
```

    ##   ScenarioId       Name Priority
    ## 1          2 Scenario 2        1
    ## 2          3 Scenario 3        2

**Note:** this will remove any previously set dependencies unless the
existing dependencies are also included in the vector.

### Multiprocessing

The [`run()`](https://syncrosim.github.io/rsyncrosim/reference/run.md)
function no longer contains the `jobs` argument for setting the number
of cores to use during a multiprocessing run. Instead, use the
`core_Multiprocessing` library datasheet to set the number of cores.

``` r
multiprocessing <- data.frame(EnableMultiprocessing = TRUE,
                              MaximumJobs = 6)
  
saveDatasheet(myLibrary, multiprocessing, "core_Multiprocessing")
```

**Note:** because the `core_Multiprocessing` datasheet is
library-scoped, modifying this datasheet will affect every scenario run.

## Deprecated functions

### Addon functions

In SyncroSim 3, the concept of *addon* packages no longer exists. All
addon packages have either been converted to standalone packages (e.g.,
`burnP3PlusPrometheus`), and can be added to a library without having to
load a base package first, or incorporated directly into its base
package (e.g., `stsimsf` is now part of `stsim`). Therefore, the
following functions have been removed:

- `addon()`
- `disableAddon()`
- `enableAddon()`

### Update functions

The following update functions have been removed:

- `updatePackage()`: You no longer need to update the installed versions
  of packages in your SyncroSim session because you can have multiple
  versions installed at the same time. To install or uninstall versions
  of a package from your SyncroSim session, use the `versions` argument
  in the
  [`installPackage()`](https://syncrosim.github.io/rsyncrosim/reference/installPackage.md)
  function instead.
- `ssimUpdate()`: To update (or downgrade) the package version on a
  library, use the `versions` argument in the `addPackages()` function
  instead.

### Raster functions

The following deprecated raster function has been removed:

- `datasheetRaster()`: This function depended on the deprecated `raster`
  package and has now been completely removed from `rsyncrosim`. Use
  [`datasheetSpatRaster()`](https://syncrosim.github.io/rsyncrosim/reference/datasheetSpatRaster.md)
  instead.

### Breakpoint functions

Setting, catching, and removing breakpoints in `rsyncrosim` scripts in
no longer supported, so the following breakpoint functions have been
removed:

- `addBreakpoint()`
- `breakpoint()`
- `deleteBreakpoint()`

## Variable naming

The primary key column for all SyncroSim datasheets has been modified
slightly. The **ID** in the primary key is now **Id** for all
datasheets. This also applies to column names in scenario-scoped
datasheets that reference values taken from project-scoped datasheets.
It is generally safe to substitute `ID` for `Id` throughout your script,
but it’s recommended to check the datasheet’s variable names.

Below is an example of a datasheet with the new naming convention for
primary keys:

``` r
datasheet(myScenario, "stsim_FlowPathway", includeKey = TRUE)
```

    ## [1] FlowPathwayId   FromStockTypeId ToStockTypeId   FlowTypeId     
    ## [5] Multiplier     
    ## <0 rows> (or 0-length row.names)

## Core datasheets

The [system
datasheets](https://docs.syncrosim.com/reference/ds_overview.html),
previously identified by the prefix `corestime_`, are now prefixed by
`core_`.

## Other NEW functions

Below is a list of other new functions that have been added to
`rsyncrosim v2`. For more information, on these new functions, refer to
the [reference
guide](https://syncrosim.github.io/rsyncrosim/reference/index.html).

``` r
# creates the conda environment for the selected package(s)
createCondaEnv()

# deletes a SyncroSim library (IRREVERSIBLE)
deleteLibrary() 

# Create, load, or modify SyncroSim charts using the new Chart class
chart() 
```
