# Sets the [`Chart`](https://syncrosim.github.io/rsyncrosim/reference/Chart-class.md) type and axes

Sets the
[`Chart`](https://syncrosim.github.io/rsyncrosim/reference/Chart-class.md)
type and adds the variables to plot in the line chart.

## Usage

``` r
chartData(
  chart,
  type = "Line",
  addX = NULL,
  addY = NULL,
  removeX = NULL,
  removeY = NULL,
  timesteps = NULL,
  iterationType = "Mean",
  iteration = 1
)

# S4 method for class 'Chart'
chartData(
  chart,
  type = "Line",
  addX = NULL,
  addY = NULL,
  removeX = NULL,
  removeY = NULL,
  timesteps = NULL,
  iterationType = "Mean",
  iteration = 1
)
```

## Arguments

- chart:

  [`Chart`](https://syncrosim.github.io/rsyncrosim/reference/Chart-class.md)
  object

- type:

  character. Chart type. Can be "Line" (Default) or "Column".

- addX:

  character or character vector. X variable(s) to add to the chart. If
  `NULL` (Default), does not add any X variables. If no X variables
  specified in chart, then will default to plotting timesteps on the X
  axis.

- addY:

  character or character vector. Y variable(s) to add to the chart. If
  `NULL` (Default), does not add any Y variables.

- removeX:

  character or character vector. X variable(s) to remove from plot. If
  `NULL` (Default), then does not remove any X variables.

- removeY:

  character or character vector. Y variable(s) to remove from plot. If
  `NULL` (Default), then does not remove any Y variables.

- timesteps:

  integer vector. The range of timesteps to plot against If `NULL`, then
  uses SyncroSim defaults.

- iterationType:

  character. How to display multiple iterations in the chart. Can be
  "Mean" (Default), "Single", or "All".

- iteration:

  integer. If the `iterationType` is set to "Single", this argument
  determines which iteration to display. Default is 1.

## Value

A `Chart` object representing a SyncroSim chart

## Examples

``` r
if (FALSE) { # \dontrun{
# Create a chart object
myChart <- chart(myProject, chart = "New Chart")

# Set the chart type and data
myChart <- chartData(myChart, y = c("variable1", "variable2"),
timesteps = c(0,10), iterationType = "single", iteration = 1)

} # }
```
