# Disaggregates the `Chart` by a Y variable

Disaggregates the `Chart` by given filter column(s) in a Y variable.

## Usage

``` r
chartDisagg(chart, variable, addFilter = NULL, removeFilter = NULL)

# S4 method for class 'Chart'
chartDisagg(chart, variable, addFilter = NULL, removeFilter = NULL)
```

## Arguments

- chart:

  `Chart` object

- variable:

  character. The variable to disaggregate the Y axis by.

- addFilter:

  character or character vector. Adds Y variable column(s) to
  disaggregate the chart by.

- removeFilter:

  character or character vector. Removes Y variable column(s) from
  disaggregating the chart.

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

# Disaggregate the chart by a filter column
myChart <- chartDisagg(myChart, variable = "variable1",
addFilter=c("col1", "col2"))

# Remove a filter column from the chart disaggregation
myChart <- chartDisagg(myChart, variable = "variable1",
removeFilter="col1")
} # }
```
