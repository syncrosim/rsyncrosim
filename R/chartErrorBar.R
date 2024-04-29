# Copyright (c) 2024 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Modify the error bars of a \code{\link{Chart}}
#'
#' Set the type and properties of the error bars of a \code{\link{Chart}}.
#'
#' @param chart \code{\link{Chart}} object
#' @param type character. Type of error bar. Values can be "percentile", 
#' "minmax", or "none". Default is NULL.
#' @param lower float. If the error bar type is set to "percentile", then
#' sets the minimum percentile for the lower range of the error bar. Default is 
#' \code{NULL}.
#' @param upper float. If the error bar type is set to "percentile", then
#' sets the maximum percentile for the upper range of the error bar. Default is 
#' \code{NULL}.
#' 
#' @return 
#' A \code{Chart} object representing a SyncroSim chart or a data.frame of
#' the current chart error bar settings.
#' 
#' @examples
#' \dontrun{
#' # Open a chart object
#' myChart <- chart(myProject, chart = "My Chart")
#' 
#' # Set the chart error bars to display the minimum/maximum of the data
#' myChart <- chartErrorBar(myChart, type = "minmax")
#' 
#' # Disable the chart error bars
#' myChart <- chartErrorBar(myChart, type = "none")
#' 
#' # Set the chart error bars to display the 95th percentile error bars
#' myChart <- chartErrorBar(myChart, type = "percentile", lower = 2.5, 
#'                          upper = 97.5)
#' }
#' 
#' @export
setGeneric("chartErrorBar", function(chart, type = NULL, lower = NULL, 
                                     upper = NULL) standardGeneric("chartErrorBar"))

#' @rdname chartErrorBar
setMethod("chartErrorBar", signature(chart = "Chart"), 
          function(chart, type, lower, upper) {
          
    # Grab project and chart ID from chart
    proj <- .project(chart)
    chartCID <- .chartId(chart)
    chartDSName <- "core_Chart"
    
    # Load chart configuration datasheet
    ds <- .datasheet(proj, name = chartDSName, optional = T, 
                     returnInvisible = T, includeKey = T)
    
    if (is.null(type)){
      ds <- ds[ds$ChartId == chartCID,]
      errorBarInfo <- data.frame(type = ds$ErrorBarType,
                                 lower = ds$ErrorBarMinPercentile,
                                 upper = ds$ErrorBarMaxPercentile)
      return(errorBarInfo)
    }
    
    # Set error bar type
    if (type == "none"){
      ds[ds$ChartId == chartCID,]$ErrorBarType <- "No Ranges"
    } else if (type == "minmax"){
      ds[ds$ChartId == chartCID,]$ErrorBarType <- "Min/Max"
    } else if (type == "percentile"){
      ds[ds$ChartId == chartCID,]$ErrorBarType <- "Percentile"
      
      # Set min / max percentiles
      if (is.numeric(lower)){
        ds[ds$ChartId == chartCID,]$ErrorBarMinPercentile <- lower
      } else {
        stop("Invalid lower percentile value:", lower)
      }
      
      if (is.numeric(upper)){
        ds[ds$ChartId == chartCID,]$ErrorBarMaxPercentile <- upper
      } else {
        stop("Invalid upper percentile value:", upper)
      }
      
    } else {
      stop(paste("Invalid error bar type:", type))
    }
    
    saveDatasheet(proj, ds, name = chartDSName, append = FALSE, force = TRUE)
    
    return(chart)
  })
