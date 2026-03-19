# Retrieves chartId of SyncroSim Chart

Retrieves the Chart Id of a SyncroSim
[`Chart`](https://syncrosim.github.io/rsyncrosim/reference/Chart-class.md).

## Usage

``` r
chartId(ssimObject)

# S4 method for class 'character'
chartId(ssimObject)

# S4 method for class 'Chart'
chartId(ssimObject)
```

## Arguments

- ssimObject:

  [`Chart`](https://syncrosim.github.io/rsyncrosim/reference/Chart-class.md)
  object

## Value

An integer: chart id.

## Examples

``` r
# \donttest{
# Set the file path and name of the new SsimLibrary
myLibraryName <- file.path(tempdir(), "testlib")

# Set the SyncroSim Session, SsimLibrary, and Project
mySession <- session()
myLibrary <- ssimLibrary(name = myLibraryName, 
                         session = mySession, 
                         packages = "stsim",
                         overwrite = TRUE) 
#> Library C:\Users\VICKIZ~1\AppData\Local\Temp\RtmpWUJoYX/testlib.ssim deleted
#> Package <stsim v4.5.3> added
myProject <- project(myLibrary, project = "Definitions")

# Get the chart object corresponding to the chart called "My Chart"
myChart <- chart(myProject, chart = "My Chart")

# Get Chart ID for SyncroSim Chart
chartId(myChart)
#> [1] 1
# }
```
