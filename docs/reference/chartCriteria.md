# Retrieves chart variables

Retrieves the available variables for charting, or the variables that
are set for an existing chart.

## Usage

``` r
chartCriteria(ssimObject, chart = NULL, variable = NULL, filter = NULL)

# S4 method for class 'SsimObject'
chartCriteria(ssimObject, chart = NULL, variable = NULL, filter = NULL)
```

## Arguments

- ssimObject:

  [`Project`](https://syncrosim.github.io/rsyncrosim/reference/Project-class.md)
  or
  [`Chart`](https://syncrosim.github.io/rsyncrosim/reference/Chart-class.md)
  object

- chart:

  character or integer. Either the name or ID of an existing chart. If
  `NULL` and a
  [`Project`](https://syncrosim.github.io/rsyncrosim/reference/Project-class.md)
  is provided as the first argument, then will return the available
  variables for charting.

- variable:

  character. The name of a charting variable. If provided, then will
  return a list of the available filter columns for that variable.
  Default is `NULL`.

- filter:

  character. The name of a filter column for a specified variable. If
  provided, then will return a list of values that pertain to the
  specified filter. If the filter column is used to disaggregate the
  chart data (using the
  [`chartDisagg`](https://syncrosim.github.io/rsyncrosim/reference/chartDisagg.md)
  function), one panel will be created for each of these values. If you
  would like to omit values from the chart, you can also add or remove
  values by the specified filter column using the
  [`chartInclude`](https://syncrosim.github.io/rsyncrosim/reference/chartInclude.md)
  function. Default is `NULL`.

## Value

A data.frame or list of variables, filter columns, and filter values.

## Details

Example arguments:

- If ssimObject is SyncroSim Project and chart is `NULL`: Returns a
  data.frame of available variables for creating a new chart.

- If ssimObject is SyncroSIm Chart or chart is not `NULL`: Returns a
  data.frame of variables in use by the specified chart.

- If variable is not `NULL`: Returns a list of filter columns that
  belong to the given variable.

- If variable and filter are not `NULL`: Returns a dataframe of value
  IDs and names that belong to the given variable and filter.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create a chart object
myChart <- chart(myProject, chart = "New Chart")

# Retrieve variables that can be used to create new charts
chartCriteria(myProject)

# Retrieve variables being used by existing chart
chartCriteria(myChart)
} # }
```
