# Modifies the legend settings for a `Chart`

Modifies the legend settings for a `Chart`.

## Usage

``` r
chartOptionsLegend(
  chart,
  show = NULL,
  showScenarioName = NULL,
  showScenarioId = NULL,
  showStageName = NULL,
  showTimestamp = NULL
)

# S4 method for class 'Chart'
chartOptionsLegend(
  chart,
  show = NULL,
  showScenarioName = NULL,
  showScenarioId = NULL,
  showStageName = NULL,
  showTimestamp = NULL
)
```

## Arguments

- chart:

  `Chart` object

- show:

  logical. Whether to show the chart legend. Default is `NULL`.

- showScenarioName:

  logical. Whether to show the scenario name in the legend. Default is
  `NULL`.

- showScenarioId:

  logical. Whether to show the scenario ID in the legend. Default is
  `NULL`.

- showStageName:

  logical. Determines whether to show the stage name (i.e., transformer
  name) in the legend. Default is `NULL`.

- showTimestamp:

  logical. Whether to show the timestamp of the scenario run in the
  legend. Default is `NULL`. Default is `NULL`.

## Value

A `Chart` object representing a SyncroSim chart or, if no arguments
other than the chart are provided, a data.frame of the current chart
legend settings.

## Examples

``` r
if (FALSE) { # \dontrun{
# Open a chart object
myChart <- chart(myProject, chart = "My Chart")

# Remove the scenario ID and the timestamp from the chart
myChart <- chartOptionsLegend(myChart, showScenarioId = FALSE, 
                              showTimestamp = FALSE)

# Hide the chart legend
myChart <- chartOptionsLegend(myChart, show = FALSE)
} # }
```
