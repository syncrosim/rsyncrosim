# Modify the X Axis of a `Chart`

Set the title and style of the X Axis of a `Chart`.

## Usage

``` r
chartOptionsXAxis(
  chart,
  title = NULL,
  numberStyle = NULL,
  decimals = NULL,
  thousandsSeparator = NULL
)

# S4 method for class 'Chart'
chartOptionsXAxis(
  chart,
  title = NULL,
  numberStyle = NULL,
  decimals = NULL,
  thousandsSeparator = NULL
)
```

## Arguments

- chart:

  `Chart` object

- title:

  character. Title of the X Axis. Default is `NULL`.

- numberStyle:

  character. Sets the style for the axes labels. Options include
  "number", scientific", or "currency". Default is `NULL`.

- decimals:

  float. Sets the number of decimal places to be displayed in the axes
  labels. Values can be between 0 and 8. Default is `NULL`.

- thousandsSeparator:

  logical. Whether to use a thousand separator (i.e., 1,000,000).
  Default is `NULL`.

## Value

A `Chart` object representing a SyncroSim chart or, if no arguments
other than the chart are provided, a data.frame of the current chart X
Axis settings.

## Examples

``` r
if (FALSE) { # \dontrun{
# Open a chart object
myChart <- chart(myProject, chart = "My Chart")

# Set the chart X Axis title
myChart <- chartOptionsXAxis(myChart, title = "Year")

# Return a dataframe of the current X Axis settings
myChart <- chartOptionsXAxis(myChart)
} # }
```
