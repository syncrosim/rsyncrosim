# Copyright (c) 2024 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Modify the X Axis of a \code{\link{Chart}}
#'
#' Set the title and style of the X Axis of a \code{\link{Chart}}.
#'
#' @param chart \code{\link{Chart}} object
#' @param title character. Title of the X Axis. Default is \code{NULL}.
#' @param numberStyle character. Sets the style for the axes labels. Options 
#' include "number", scientific", or "currency". Default is \code{NULL}.
#' @param decimals float. Sets the number of decimal places to be displayed in 
#' the axes labels. Values can be between 0 and 8. Default is \code{NULL}.
#' @param thousandsSeparator logical. Whether to use a thousand separator 
#' (i.e., 1,000,000). Default is \code{NULL}.
#' 
#' @return 
#' A \code{Chart} object representing a SyncroSim chart or, if no arguments 
#' other than the chart are provided, a data.frame of the current chart X Axis 
#' settings.
#' 
#' @examples
#' \dontrun{
#' # Open a chart object
#' myChart <- chart(myProject, chart = "My Chart")
#' 
#' # Set the chart X Axis title
#' myChart <- chartXAxis(myChart, title = "Year")
#' 
#' # Return a dataframe of the current X Axis settings
#' myChart <- chartXAxis(myChart)
#' }
#' 
#' @export
setGeneric("chartXAxis", function(
    chart, title = NULL, numberStyle = NULL, decimals = NULL, 
    thousandsSeparator = NULL) standardGeneric("chartXAxis"))

#' @rdname chartXAxis
setMethod("chartXAxis", signature(chart = "Chart"), 
          function(chart, title, numberStyle, decimals, thousandsSeparator) {
            
  # Grab project and chart ID from chart
  proj <- .project(chart)
  chartCID <- .chartId(chart)
  chartDSName <- "core_Chart"
  
  # Load chart configuration datasheet
  ds <- .datasheet(proj, name = chartDSName, optional = T, 
                   returnInvisible = T, includeKey = T)
  
  # Set X Axis title
  allNULL <- TRUE
  if (!is.null(title) && is.character(title)){
    ds[ds$ChartId == chartCID,]$ChartXAxisTitle <- title
    allNULL <- FALSE
  } 
  
  if (!is.null(numberStyle)){
    allNULL <- FALSE
    if (numberStyle == "number"){
      ds[ds$ChartId == chartCID,]$PanelXAxisNumberStyle <- "Number"
    } else if (numberStyle == "scientific"){
      ds[ds$ChartId == chartCID,]$PanelXAxisNumberStyle <- "Scientific"
    } else if (numberStyle == "currency"){
      ds[ds$ChartId == chartCID,]$PanelXAxisNumberStyle <- "Currency"
    } else {
      stop("numberStyle should be one of 'number', 'scientific', or 'currency'")
    }
  }
  
  if (!is.null(decimals) && is.numeric(decimals)){
    allNULL <- FALSE
    if ((decimals >= 0) && (decimals <= 8)){
      ds[ds$ChartId == chartCID,]$PanelYAxisDecimalPlaces <- decimals
    } else {
      stop("decimals should be an integer between 0 and 8.")
    }
  }
  
  if (!is.null(thousandsSeparator)){
    allNULL <- FALSE
    if (is.logical(thousandsSeparator)){
      ds[ds$ChartId == chartCID,]$PanelXAxisShowThousandsSep <- thousandsSeparator
    } else {
      stop("thousandsSeparator should be logical.")
    }
  }
  
  if (allNULL){
    ds <- ds[ds$ChartId == chartCID,]
    XAxisSettings <- data.frame(title = ds$ChartXAxisTitle,
                                numberStyle = ds$PanelXAxisNumberStyle,
                                decimals = ds$PanelXAxisDecimalPlaces,
                                thousandsSeparator = ds$PanelXAxisShowThousandsSep)
    return(XAxisSettings)
  }
  
  saveDatasheet(proj, ds, name = chartDSName, append = FALSE, force = TRUE)
  
  return(chart)
})
