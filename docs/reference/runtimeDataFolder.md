# SyncroSim Data Folder

This function is part of a set of functions designed to facilitate the
development of R-based Syncrosim Packages. This function creates and
returns a SyncroSim Data Folder.

## Usage

``` r
runtimeDataFolder(scenario, datasheetName)
```

## Arguments

- scenario:

  `Scenario` object. A SyncroSim result Scenario

- datasheetName:

  character. The datasheet name

## Value

Returns a data folder name for the specified datasheet.

## Examples

``` r
if (FALSE) { # \dontrun{
dataFolder <- runtimeDataFolder()
} # }
```
