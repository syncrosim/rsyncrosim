# Modifies the font settings for a `Chart`

Modifies the font style and size of various `Chart` components.

## Usage

``` r
chartOptionsFont(
  chart,
  titleFont = NULL,
  titleStyle = NULL,
  titleSize = NULL,
  panelFont = NULL,
  panelStyle = NULL,
  panelSize = NULL,
  axisFont = NULL,
  axisStyle = NULL,
  axisSize = NULL,
  legendFont = NULL,
  legendStyle = NULL,
  legendSize = NULL
)

# S4 method for class 'Chart'
chartOptionsFont(
  chart,
  titleFont = NULL,
  titleStyle = NULL,
  titleSize = NULL,
  panelFont = NULL,
  panelStyle = NULL,
  panelSize = NULL,
  axisFont = NULL,
  axisStyle = NULL,
  axisSize = NULL,
  legendFont = NULL,
  legendStyle = NULL,
  legendSize = NULL
)
```

## Arguments

- chart:

  `Chart` object

- titleFont:

  character. Sets the font for the title of the chart axes (e.g.,
  "Microsoft Sans Serif, "Times New Roman", "Arial Narrow"). Default is
  `NULL`.

- titleStyle:

  character. Sets the font style for the title. Values can be
  "standard", "italic", "bold", or "bold/italic". Default is `NULL`.

- titleSize:

  integer. Sets the font size for the title of the chart axes. Default
  is `NULL`.

- panelFont:

  character. Sets the font for the title of the chart panels (e.g.,
  "Microsoft Sans Serif, "Times New Roman", "Arial"). Default is `NULL`.

- panelStyle:

  character. Sets the font style for the chart panels. Values can be
  "standard", "italic", "bold", or "bold/italic". Default is `NULL`.

- panelSize:

  integer. Sets the font size for the chart panels. Default is `NULL`.

- axisFont:

  character. Sets the font for the title of the chart panel axes (e.g.,
  "Microsoft Sans Serif, "Times New Roman", "Arial"). Default is `NULL`.

- axisStyle:

  character. Sets the font style for the chart panel axes. Values can be
  "standard", "italic", "bold", or "bold/italic". Default is `NULL`.

- axisSize:

  integer. Sets the font size for the chart panel axes. Default is
  `NULL`.

- legendFont:

  character. Sets the font for the title of the chart legend (e.g.,
  "Microsoft Sans Serif, "Times New Roman", "Arial"). Default is `NULL`.

- legendStyle:

  character. Sets the font style for the chart legend. Values can be
  "standard", "italic", "bold", or "bold/italic". Default is `NULL`.

- legendSize:

  integer. Sets the font size for the chart legend. Default is `NULL`.

## Value

A `Chart` object representing a SyncroSim chart or, if no arguments
other than the chart are provided, a data.frame of the current chart
font settings.

## Examples

``` r
if (FALSE) { # \dontrun{
# Open a chart object
myChart <- chart(myProject, chart = "My Chart")

# Set the font for the chart panels
myChart <- chartOptionsFont(myChart, panelFont = "Microsoft Sans Serif", 
                            panelStyle = "bold/italic", panelSize = 8)

# Return a dataframe of the current font settings
myChart <- chartOptionsFont(myChart)
} # }
```
