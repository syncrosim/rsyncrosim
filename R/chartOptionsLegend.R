# Copyright (c) 2024 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Modifies the legend settings for a \code{\link{Chart}}
#'
#' Modifies the legend settings for a \code{\link{Chart}}.
#'
#' @param chart \code{\link{Chart}} object
#' @param show logical. Whether to show the chart legend. Default is \code{NULL}.
#' @param showScenarioName logical. Whether to show the scenario name in the 
#'    legend. Default is \code{NULL}.
#' @param showScenarioID logical. Whether to show the scenario ID in the legend.
#'    Default is \code{NULL}.
#' @param showStageName logical. Determines whether to show the stage name 
#'    (i.e., transformer name) in the legend. Default is \code{NULL}.
#' @param showTimestamp logical. Whether to show the timestamp of the scenario 
#'    run in the legend. Default is \code{NULL}. Default is \code{NULL}.
#' 
#' @return 
#' A \code{Chart} object representing a SyncroSim chart or, if no arguments 
#' other than the chart are provided, a data.frame of the current chart legend 
#' settings.
#' 
#' @examples
#' \dontrun{
#' # Open a chart object
#' myChart <- chart(myProject, chart = "My Chart")
#' 
#' # Remove the scenario ID and the timestamp from the chart
#' myChart <- chartOptionsLegend(myChart, showScenarioID = FALSE, 
#'                               showTimestamp = FALSE)
#' 
#' # Hide the chart legend
#' myChart <- chartOptionsLegend(myChart, show = FALSE)
#' }
#' 
#' @export
setGeneric("chartOptionsLegend", function(
    chart, show = NULL, showScenarioName = NULL, showScenarioID = NULL, 
    showStageName = NULL, showTimestamp = NULL) standardGeneric("chartOptionsLegend"))

#' @rdname chartOptionsLegend
setMethod("chartOptionsLegend", signature(chart = "Chart"), 
          function(chart, show, showScenarioName, showScenarioID, 
                   showStageName, showTimestamp) {
  
  # Grab project and chart ID from chart
  proj <- .project(chart)
  chartCID <- .chartId(chart)
  chartDSName <- "core_Chart"
  
  # Load chart configuration datasheet
  ds <- .datasheet(proj, name = chartDSName, optional = T, 
                   returnInvisible = T, includeKey = T, verbose = F)
  
  # Set variable for checking if we should just return a dataframe of settings
  allNULL <- TRUE
  
  # Set legend options
  if (!is.null(show)){
    allNULL <- FALSE
    if (is.logical(show)){
      ds[ds$ChartId == chartCID,]$ChartShowLegend <- show
    } else {
      stop("show should be logical.")
    }
  }
  
  if (!is.null(showScenarioName)){
    allNULL <- FALSE
    if (is.logical(showScenarioName)){
      ds[ds$ChartId == chartCID,]$ChartLegendShowScenarioName <- showScenarioName
    } else {
      stop("showScenarioName should be logical.")
    }
  }
  
  if (!is.null(showScenarioID)){
    allNULL <- FALSE
    if (is.logical(showScenarioID)){
      ds[ds$ChartId == chartCID,]$ChartLegendShowScenarioId <- showScenarioID
    } else {
      stop("showScenarioID should be logical.")
    }
  }
  
  if (!is.null(showStageName)){
    allNULL <- FALSE
    if (is.logical(showStageName)){
      ds[ds$ChartId == chartCID,]$ChartLegendShowStageName <- showStageName
    } else {
      stop("showStageName should be logical.")
    }
  }
  
  if (!is.null(showTimestamp)){
    allNULL <- FALSE
    if (is.logical(showTimestamp)){
      ds[ds$ChartId == chartCID,]$ChartLegendShowTimestamp <- showTimestamp
    } else {
      stop("showTimestamp should be logical.")
    }
  }
  
  if (allNULL){
    ds <- ds[ds$ChartId == chartCID,]
    legendOptions <- data.frame(show = ds$ChartShowLegend,
                                showScenarioName = ds$ChartLegendShowScenarioName,
                                showScenarioID = ds$ChartLegendShowScenarioId,
                                showStageName = ds$ChartLegendShowStageName,
                                showTimestamp = ds$ChartLegendShowTimestamp)
    
    return(legendOptions)
  }
  
  saveDatasheet(proj, ds, name = chartDSName, append = FALSE, force = TRUE)
  
  return(chart)
})