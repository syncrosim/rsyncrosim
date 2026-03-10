# Add or remove values by column in a [`Chart`](https://syncrosim.github.io/rsyncrosim/reference/Chart-class.md)

Add or remove values by a specified column in the X or Y axis of a
[`Chart`](https://syncrosim.github.io/rsyncrosim/reference/Chart-class.md).

## Usage

``` r
chartInclude(
  chart,
  variable,
  filter,
  axis = "Y",
  addValue = NULL,
  removeValue = NULL
)

# S4 method for class 'Chart'
chartInclude(
  chart,
  variable,
  filter,
  axis = "Y",
  addValue = NULL,
  removeValue = NULL
)
```

## Arguments

- chart:

  [`Chart`](https://syncrosim.github.io/rsyncrosim/reference/Chart-class.md)
  object

- variable:

  character. A variable belonging to the X or Y axis.

- filter:

  character or character vector. A filter column belonging to the X or Y
  variable.

- axis:

  character. Either "X" or "Y" corresponding to the X or Y axis of the
  chart. Default is "Y".

- addValue:

  character or character vector. Adds value(s) from the specified filter
  column and X or Y variable to be included in the chart.

- removeValue:

  character or character vector. Removes value(s) from the specified
  filter column and X or Y variable from being included in the chart.

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

# Include specific values in the chart
myChart <- chartInclude(myChart, variable = "variable1",
filter="col1", addValue=c("val1", "val2", "val3"))

# Remove specific values from the chart
myChart <- chartInclude(myChart, variable = "variable1",
filter="col1", removeValue="val3")
} # }
```
