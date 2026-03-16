# Modify the error bars of a [`Chart`](https://syncrosim.github.io/rsyncrosim/reference/Chart-class.md)

Set the type and properties of the error bars of a
[`Chart`](https://syncrosim.github.io/rsyncrosim/reference/Chart-class.md).

## Usage

``` r
chartErrorBar(chart, type = NULL, lower = NULL, upper = NULL)

# S4 method for class 'Chart'
chartErrorBar(chart, type = NULL, lower = NULL, upper = NULL)
```

## Arguments

- chart:

  [`Chart`](https://syncrosim.github.io/rsyncrosim/reference/Chart-class.md)
  object

- type:

  character. Type of error bar. Values can be "percentile", "minmax", or
  "none". Default is NULL.

- lower:

  float. If the error bar type is set to "percentile", then sets the
  minimum percentile for the lower range of the error bar. Default is
  `NULL`.

- upper:

  float. If the error bar type is set to "percentile", then sets the
  maximum percentile for the upper range of the error bar. Default is
  `NULL`.

## Value

A `Chart` object representing a SyncroSim chart or, if no arguments
other than the chart are provided, a data.frame of the current chart
error bar settings.

## Examples

``` r
if (FALSE) { # \dontrun{
# Open a chart object
myChart <- chart(myProject, chart = "My Chart")

# Set the chart error bars to display the minimum/maximum of the data
myChart <- chartErrorBar(myChart, type = "minmax")

# Disable the chart error bars
myChart <- chartErrorBar(myChart, type = "none")

# Set the chart error bars to display the 95th percentile error bars
myChart <- chartErrorBar(myChart, type = "percentile", lower = 2.5, 
                         upper = 97.5)
} # }
```
