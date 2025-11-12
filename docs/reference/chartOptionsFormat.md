# Modifies the font settings for a `Chart`

Modifies the font style and size of various `Chart` components.

## Usage

``` r
chartOptionsFormat(
  chart,
  noDataAsZero = NULL,
  showDataPoints = NULL,
  showDataPointsOnly = NULL,
  showPanelTitles = NULL,
  showToolTips = NULL,
  showNoDataPanels = NULL,
  lineWidth = NULL
)

# S4 method for class 'Chart'
chartOptionsFormat(
  chart,
  noDataAsZero = NULL,
  showDataPoints = NULL,
  showDataPointsOnly = NULL,
  showPanelTitles = NULL,
  showToolTips = NULL,
  showNoDataPanels = NULL,
  lineWidth = NULL
)
```

## Arguments

- chart:

  `Chart` object

- noDataAsZero:

  logical. Determines whether NA, Null and No Data values should be
  charted as zero. Default is `NULL`.

- showDataPoints:

  logical. Determines whether each data point should be displayed with a
  point (i.e., circle). Default is `NULL`.

- showDataPointsOnly:

  logical. Determines whether only points should be displayed (i.e., no
  line in the line charts). Default is `NULL`.

- showPanelTitles:

  logical. Determines whether to show a title above each panel. Default
  is `NULL`.

- showToolTips:

  logical. Determines whether to show the tool tip when hovering the
  cursor over a data point. Default is `NULL`.

- showNoDataPanels:

  logical. Determines whether to show chart panels with no data. Default
  is `NULL`.

- lineWidth:

  integer. Sets the charts' line thickness. Default is `NULL`.

## Value

A `Chart` object representing a SyncroSim chart or, if no arguments
other than the chart are provided, a data.frame of the current chart
format settings.

## Examples

``` r
if (FALSE) { # \dontrun{
# Open a chart object
myChart <- chart(myProject, chart = "My Chart")

# Set the format for the chart panels
myChart <- chartOptionsFormat(myChart, noDataAsZero = TRUE, 
                              showDataPoints = FALSE, 
                              showDataPointsOnly = FALSE,
                              showPanelTitles = TRUE,
                              showToolTips = TRUE,
                              showNoDataPanels = FALSE,
                              lineWidth = 1)

# Return a dataframe of the current font settings
myChart <- chartOptionsFormat(myChart)
} # }
```
