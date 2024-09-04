# Copyright (c) 2024 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Modify the Y axis of a \code{\link{Chart}}
#'
#' Set the title and style of the Y axis of a \code{\link{Chart}}.
#'
#' @param chart \code{\link{Chart}} object
#' @param title character. Title of the Y axis. Default is \code{NULL}.
#' @param numberStyle character. Sets the style for the axes labels. Options 
#' include "number", scientific", or "currency". Default is \code{NULL}.
#' @param decimals float. Sets the number of decimal places to be displayed in 
#' the axes labels. Values can be between 0 and 8. Default is \code{NULL}.
#' @param thousandsSeparator logical. Whether to use a thousand separator 
#' (i.e., 1,000,000). Default is \code{NULL}.
#' @param minZero logical. Whether the minimum value displayed in the Y axis 
#' should be zero.
#' @param sameScale logical. Whether the Y axis scale should be consistent 
#' across chart panels. Default is \code{NULL}.
#' @param fixedIntervals logical. Whether the interval between Y axis labels 
#' should be consistent across chart panels. Default is \code{NULL}.
#' 
#' @return 
#' A \code{Chart} object representing a SyncroSim chart or, if no arguments 
#' other than the chart are provided, a data.frame of the current chart Y axis 
#' settings.
#' 
#' @examples
#' \dontrun{
#' # Open a chart object
#' myChart <- chart(myProject, chart = "My Chart")
#' 
#' # Set the chart Y axis title
#' myChart <- chartOptionsYAxis(myChart, title = "Year")
#' 
#' # Return a dataframe of the current Y axis settings
#' myChart <- chartOptionsYAxis(myChart)
#' }
#' 
#' @export
setGeneric("chartOptionsYAxis", function(
    chart, title = NULL, numberStyle = NULL, decimals = NULL, 
    thousandsSeparator = NULL, minZero = NULL, sameScale = NULL, 
    fixedIntervals = NULL) standardGeneric("chartOptionsYAxis"))

#' @rdname chartOptionsYAxis
setMethod("chartOptionsYAxis", signature(chart = "Chart"), 
          function(chart, title, numberStyle, decimals, thousandsSeparator, 
                   minZero, sameScale, fixedIntervals) {
            
  # Grab project and chart ID from chart
  proj <- .project(chart)
  chartCID <- .chartId(chart)
  chartDSName <- "core_Chart"
  
  # Load chart configuration datasheet
  ds <- .datasheet(proj, name = chartDSName, optional = T, 
                   returnInvisible = T, includeKey = T, verbose = F)
  
  # Set Y axis title
  allNULL <- TRUE
  if (!is.null(title) && is.character(title)){
    ds[ds$ChartId == chartCID,]$ChartYAxisTitle <- title
    allNULL <- FALSE
  } 
  
  if (!is.null(numberStyle)){
    allNULL <- FALSE
    if (numberStyle == "number"){
      ds[ds$ChartId == chartCID,]$PanelYAxisNumberStyle <- "Number"
    } else if (numberStyle == "scientific"){
      ds[ds$ChartId == chartCID,]$PanelYAxisNumberStyle <- "Scientific"
    } else if (numberStyle == "currency"){
      ds[ds$ChartId == chartCID,]$PanelYAxisNumberStyle <- "Currency"
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
      ds[ds$ChartId == chartCID,]$PanelYAxisShowThousandsSep <- thousandsSeparator
    } else {
      stop("thousandsSeparator should be logical.")
    }
  }
  
  if (!is.null(minZero)){
    allNULL <- FALSE
    if (is.logical(minZero)){
      ds[ds$ChartId == chartCID,]$PanelYAxisMinZero <- minZero
    } else {
      stop("minZero should be logical.")
    }
  }
  
  if (!is.null(sameScale)){
    allNULL <- FALSE
    if (is.logical(sameScale)){
      ds[ds$ChartId == chartCID,]$PanelYAxisSameScale <- sameScale
    } else {
      stop("sameScale should be logical.")
    }
  }
  
  if (!is.null(fixedIntervals)){
    allNULL <- FALSE
    if (is.logical(fixedIntervals)){
      ds[ds$ChartId == chartCID,]$PanelFixedYAxisIntervals <- fixedIntervals
    } else {
      stop("fixedIntervals should be logical.")
    }
  }
  
  if (allNULL){
    ds <- ds[ds$ChartId == chartCID,]
    yAxisSettings <- data.frame(title = ds$ChartYAxisTitle,
                                numberStyle = ds$PanelYAxisNumberStyle,
                                decimals = ds$PanelYAxisDecimalPlaces,
                                thousandsSeparator = ds$PanelYAxisShowThousandsSep,
                                minZero = ds$PanelYAxisMinZero,
                                sameScale = ds$PanelYAxisSameScale,
                                fixedIntervals = ds$PanelFixedYAxisIntervals)
    return(yAxisSettings)
  }
  
  saveDatasheet(proj, ds, name = chartDSName, append = FALSE, force = TRUE)
  
  return(chart)
})
