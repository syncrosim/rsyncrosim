# Modify the Y axis of a [`Chart`](https://syncrosim.github.io/rsyncrosim/reference/Chart-class.md)

Set the title and style of the Y axis of a
[`Chart`](https://syncrosim.github.io/rsyncrosim/reference/Chart-class.md).

## Usage

``` r
chartOptionsYAxis(
  chart,
  title = NULL,
  numberStyle = NULL,
  decimals = NULL,
  thousandsSeparator = NULL,
  minZero = NULL,
  sameScale = NULL,
  fixedIntervals = NULL
)

# S4 method for class 'Chart'
chartOptionsYAxis(
  chart,
  title = NULL,
  numberStyle = NULL,
  decimals = NULL,
  thousandsSeparator = NULL,
  minZero = NULL,
  sameScale = NULL,
  fixedIntervals = NULL
)
```

## Arguments

- chart:

  [`Chart`](https://syncrosim.github.io/rsyncrosim/reference/Chart-class.md)
  object

- title:

  character. Title of the Y axis. Default is `NULL`.

- numberStyle:

  character. Sets the style for the axes labels. Options include
  "number", scientific", or "currency". Default is `NULL`.

- decimals:

  float. Sets the number of decimal places to be displayed in the axes
  labels. Values can be between 0 and 8. Default is `NULL`.

- thousandsSeparator:

  logical. Whether to use a thousand separator (i.e., 1,000,000).
  Default is `NULL`.

- minZero:

  logical. Whether the minimum value displayed in the Y axis should be
  zero.

- sameScale:

  logical. Whether the Y axis scale should be consistent across chart
  panels. Default is `NULL`.

- fixedIntervals:

  logical. Whether the interval between Y axis labels should be
  consistent across chart panels. Default is `NULL`.

## Value

A `Chart` object representing a SyncroSim chart or, if no arguments
other than the chart are provided, a data.frame of the current chart Y
axis settings.

## Examples

``` r
if (FALSE) { # \dontrun{
# Open a chart object
myChart <- chart(myProject, chart = "My Chart")

# Set the chart Y axis title
myChart <- chartOptionsYAxis(myChart, title = "Year")

# Return a dataframe of the current Y axis settings
myChart <- chartOptionsYAxis(myChart)
} # }
```
