### ApexRMS
### 2024-09-20
### Below script tests the following functions:
### * chart
### * chartCriteria
### * chartData
### * chartDisagg
### * chartInclude
### * chartErrorBar
### * chartOptions
### * chartOptionsFont
### * chartOptionsLegend
### * chartOptionsFormat
### * readOnly
### * name
### * delete

# Setup ----
library(rsyncrosim)

# set up library
mySession <- session("C:/Program Files/SyncroSim Studio")
libPath <- "tests_in_progress/test_library/chart-testing.ssim"

myLibrary <- ssimLibrary(name = libPath,
                         session = mySession)

# define project
myProject <- rsyncrosim::project(myLibrary, project = 1)

# define scenario
scenario(myProject)
myScenario <- scenario(myProject, scenario = "Snow cover")

# view datasheets
datasheet(myScenario)

# Tests ----

# testing chart function --

# get list of charts in the project
chart(myProject)

# create a new chart in the project
chart(myScenario, chart = "Test Chart")

# get list of charts in the scenario
chart(myScenario)

# define chart to test with
myChart <- chart(myScenario,
                 chart = "Test Chart")

# testing chartCriteria function --

# get information about Test Chart
chartCriteria(myChart)

# testing chartData function --

# Change chart type to "column"
myChart <- chartData(myChart,
                     type = "Column")
chart(myChart)

# Change iteration type to "single"
myChart <- chartData(myChart,
                     iterationType = "Single")

# set display iteration to 2
myChart <- chartData(myChart,
                     iteration = 2)

# add variables
myChart <- chartData(myChart,
                     addX = "stsim_Transition")
myChart <- chartData(myChart,
                     addY = "stsim_StateClass")

# remove variables
myChart <- chartData(myChart,
                     removeX = "stsim_Transition")
myChart <- chartData(myChart,
                     removeY = "stsim_StateClass")

# set timesteps
myChart <- chartData(myChart,
                     timesteps = c(1, 30))

# all arguments together
myChart <- chartData(myChart,
                     type = "Column",
                     addX = "stsim_Transition",
                     addY = "stsim_StateClass",
                     timesteps = c(1, 40),
                     iterationType = "All",
                     iteration = 1)

# view chart information
chart(myChart)

# testing chartDisagg function --
# look up potential filter columns
datasheet(myScenario, name = "stsim_StateClass")

# specify variable to disaggregate by
myChart <- chartDisagg(myChart,
                       variable = "stsim_StateClass") # requires addFilter argument

# specify a filter column
myChart <- chartDisagg(myChart, variable = "stsim_StateClass",
                       addFilter = "StateLabelXId")

myChart <- chartDisagg(myChart, variable = "stsim_StateClass",
                       addFilter = "StateLabelYId")

# remove a filter column
myChart <- chartDisagg(myChart, variable = "stsim_StateClass",
                       removeFilter = "StateLabelXId")

# view chart information
chart(myChart)

# testing chartInclude function --
# add values from the selected variable to include in the chart
myChart <- chartInclude(myChart,
                        variable = "stsim_StateClass",
                        filter = "StateLabelXId",
                        addValue = c("Coniferous", "Mixed")) # not working

# remove values
myChart <- chartInclude(myChart,
                        variable = "stsim_StateClass",
                        filter = "StateLabelXId",
                        removeValue = "Deciduous") # not working

# testing chartErrorBar function --
# set the chart error bar to "minmax"
myChart <- chartErrorBar(myChart,
                         type = "minmax")

# set the chart error bar to "percentile"
myChart <- chartErrorBar(myChart,
                         type = "none")

# set the chart error bar to "percentile"
myChart <- chartErrorBar(myChart,
                         type = "percentile",
                         lower = 0.25,
                         upper = 97.5)

# testing chartOptionsXAxis function -
# Change chart type back to "Line"
myChart <- chartData(myChart,
                     type = "Line")

# add x axis title and format axis
myChart <- chartOptionsXAxis(myChart,
                             title = "X axis",
                             numberStyle = "number",
                             decimals = 2,
                             thousandsSeparator = TRUE)

# change x axis title and formatting
myChart <- chartOptionsXAxis(myChart,
                             title = "X axis title",
                             numberStyle = "currency",
                             decimals = 1,
                             thousandsSeparator = FALSE)

# change numberStyle and decimals
myChart <- chartOptionsXAxis(myChart,
                             title = "X axis title",
                             numberStyle = "scientific",
                             decimals = 3,
                             thousandsSeparator = FALSE)

# testing chartOptionsYAxis function --
# disaggregate to test y axis formatting
myChart <- chartDisagg(myChart, variable = "stsim_StateClass",
                       addFilter = "StateLabelXId")

# add y axis title and format axis
myChart <- chartOptionsYAxis(myChart,
                             title = "Y axis",
                             numberStyle = "number",
                             decimals = 2,
                             thousandsSeparator = FALSE,
                             minZero = FALSE,
                             sameScale = FALSE,
                             fixedIntervals = FALSE)

# change numberStyle, decimals, and minZero
myChart <- chartOptionsYAxis(myChart,
                             title = "Y axis",
                             numberStyle = "currency",
                             decimals = 1,
                             thousandsSeparator = FALSE,
                             minZero = TRUE,
                             sameScale = FALSE,
                             fixedIntervals = FALSE)

# change sameScale to TRUE
myChart <- chartOptionsYAxis(myChart,
                             title = "Y axis",
                             numberStyle = "scientific",
                             decimals = 1,
                             thousandsSeparator = FALSE,
                             minZero = FALSE,
                             sameScale = TRUE,
                             fixedIntervals = FALSE)

# testing chartOptionsFont function --
myChart <- chartOptionsFont(myChart,
                            titleFont = "Times New Roman",
                            titleStyle = "standard",
                            titleSize = 6,
                            panelFont = "Times New Roman",
                            panelStyle = "standard",
                            panelSize = 6,
                            axisFont = "Times New Roman",
                            axisStyle = "standard",
                            axisSize = 6,
                            legendFont = "Times New Roman",
                            legendStyle = "standard",
                            legendSize = 6)

# switch to font = Arial, style = bold, and size = 8
myChart <- chartOptionsFont(myChart,
                            titleFont = "Arial",
                            titleStyle = "bold",
                            titleSize = 8,
                            panelFont = "Arial",
                            panelStyle = "bold",
                            panelSize = 8,
                            axisFont = "Arial",
                            axisStyle = "bold",
                            axisSize = 8,
                            legendFont = "Arial",
                            legendStyle = "bold",
                            legendSize = 9)

# testing chartOptionsLegend function --
# set all options to TRUE
myChart <- chartOptionsLegend(myChart,
                              show = TRUE,
                              showScenarioName = TRUE,
                              showScenarioID = TRUE,
                              showStageName = TRUE,
                              showTimestamp = TRUE)

# remove scenario name
myChart <- chartOptionsLegend(myChart,
                              show = TRUE,
                              showScenarioName = FALSE,
                              showScenarioID = TRUE,
                              showStageName = TRUE,
                              showTimestamp = TRUE)

# set remove everyhtin except scenario name
myChart <- chartOptionsLegend(myChart,
                              show = TRUE,
                              showScenarioName = TRUE,
                              showScenarioID = FALSE,
                              showStageName = FALSE,
                              showTimestamp = FALSE)

# remove legend
myChart <- chartOptionsLegend(myChart,
                              show = FALSE)

# testing chartOptionsFormat function --
# increase lineWidth, set everything to TRUE except showDataPointsOnly
myChart <- chartOptionsFormat(myChart,
                              noDataAsZero = TRUE,
                              showDataPoints = TRUE,
                              showDataPointsOnly = FALSE,
                              showPanelTitles  = TRUE,
                              showToolTips = TRUE,
                              showNoDataPanels = TRUE,
                              lineWidth = 4)

# set showDataPointsOnly to TRUE
myChart <- chartOptionsFormat(myChart,
                              noDataAsZero = TRUE,
                              showDataPoints = TRUE,
                              showDataPointsOnly = TRUE,
                              showPanelTitles  = TRUE,
                              showToolTips = TRUE,
                              showNoDataPanels = TRUE,
                              lineWidth = 4)

# remove data points, set all arguments to FALSE, decrease lineWidth
myChart <- chartOptionsFormat(myChart,
                              noDataAsZero = FALSE,
                              showDataPoints = FALSE,
                              showDataPointsOnly = FALSE,
                              showPanelTitles  = FALSE,
                              showToolTips = FALSE,
                              showNoDataPanels = FALSE,
                              lineWidth = 1)

# testing readOnly function --
# get read only status
readOnly(myChart)

# set read only to TRUE
readOnly(myChart) <- TRUE

# confirm
readOnly(myChart)

# testing name function --
# get name of chart
name(myChart)

# rename
name(myChart) <- "Test Chart Renamed"

# confirm new name
name(myChart) # did not assign new name

# testing delete function --
delete(myChart) # this works
delete(chart = "Test Chart") # this does not work
