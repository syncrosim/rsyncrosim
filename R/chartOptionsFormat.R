# Copyright (c) 2024 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Modifies the font settings for a \code{\link{Chart}}
#'
#' Modifies the font style and size of various \code{\link{Chart}} components.
#'
#' @param chart \code{\link{Chart}} object
#' @param noDataAsZero logical. Determines whether NA, Null and No Data values 
#' should be charted as zero. Default is \code{NULL}.
#' @param showDataPoints logical. Determines whether each data point should be 
#' displayed with a point (i.e., circle). Default is \code{NULL}. 
#' @param showDataPointsOnly logical. Determines whether only points should be 
#' displayed (i.e., no line in the line charts). Default is \code{NULL}.
#' @param showPanelTitles logical. Determines whether to show a title above each 
#' panel. Default is \code{NULL}.
#' @param showToolTips logical. Determines whether to show the tool tip when 
#' hovering the cursor over a data point. Default is \code{NULL}. 
#' @param showNoDataPanels logical. Sets the font size for the chart panels. 
#' Default is \code{NULL}.
#' @param lineWidth integer. Sets the charts' line thickness. Default 
#' is \code{NULL}.
#' 
#' @return 
#' A \code{Chart} object representing a SyncroSim chart or, if no arguments 
#' other than the chart are provided, a data.frame of the current chart format 
#' settings.
#' 
#' @examples
#' \dontrun{
#' # Open a chart object
#' myChart <- chart(myProject, chart = "My Chart")
#' 
#' # Set the format for the chart panels
#' myChart <- chartOptionsFormat(myChart, noDataAsZero = TRUE, 
#'                               showDataPoints = FALSE, 
#'                               showDataPointsOnly = FALSE,
#'                               showPanelTitles = TRUE,
#'                               showToolTips = TRUE,
#'                               showNoDataPanels = FALSE,
#'                               lineWidth = 1)
#' 
#' # Return a dataframe of the current font settings
#' myChart <- chartOptionsFormat(myChart)
#' }
#' 
#' @export
setGeneric("chartOptionsFormat", function(
    chart, noDataAsZero = NULL, showDataPoints = NULL, 
    showDataPointsOnly = NULL, showPanelTitles = NULL, showToolTips = NULL, 
    showNoDataPanels = NULL, lineWidth = NULL
    ) standardGeneric("chartOptionsFormat"))

#' @rdname chartOptionsFormat
setMethod("chartOptionsFormat", signature(chart = "Chart"), 
          function(chart, noDataAsZero, showDataPoints, showDataPointsOnly, 
                   showPanelTitles, showToolTips, showNoDataPanels, 
                   lineWidth) {
            
            # Grab project and chart ID from chart
            proj <- .project(chart)
            chartCID <- .chartId(chart)
            chartDSName <- "core_Chart"
            
            # Load chart configuration datasheet
            ds <- .datasheet(proj, name = chartDSName, optional = T, 
                             returnInvisible = T, includeKey = T, verbose = F)
            
            # Set variable to check if we should return a dataframe of settings
            allNULL <- TRUE
            
            if (!is.null(noDataAsZero)){
              allNULL <- FALSE
              ds[ds$ChartId == chartCID,]$PanelNoDataAsZero <- noDataAsZero
            } 
            
            if (!is.null(showDataPoints)){
              allNULL <- FALSE
              ds[ds$ChartId == chartCID,]$PanelShowDataPoints <- showDataPoints
            } 
            
            if (!is.null(showDataPointsOnly)){
              allNULL <- FALSE
              ds[ds$ChartId == chartCID,]$PanelShowDataPointsOnly <- showDataPointsOnly
            } 
            
            if (!is.null(showPanelTitles)){
              allNULL <- FALSE
              ds[ds$ChartId == chartCID,]$PanelShowTitles <- showPanelTitles
            }          
            
            if (!is.null(showToolTips)){
              allNULL <- FALSE
              ds[ds$ChartId == chartCID,]$PanelShowToolTips <- showToolTips
            }
            
            if (!is.null(showNoDataPanels)){
              allNULL <- FALSE
              ds[ds$ChartId == chartCID,]$PanelShowNoDataPanels <- showNoDataPanels
            }
            
            if (!is.null(lineWidth)){
              allNULL <- FALSE
              ds[ds$ChartId == chartCID,]$PanelLineThickness <- lineWidth
            }
            
            if (allNULL){
              ds <- ds[ds$ChartId == chartCID,]
              formatSettings <- data.frame(
                noDataAsZero = ds$PanelNoDataAsZero,
                showDataPoints = ds$PanelShowDataPoints,
                showDataPointsOnly = ds$PanelShowDataPointsOnly,
                showPanelTitles = ds$PanelShowTitles,
                showToolTips = ds$PanelShowToolTips,
                showNoDataPanels = ds$PanelShowNoDataPanels,
                lineWidth = ds$PanelLineThickness)
              
              return(formatSettings)
            }
            
            saveDatasheet(proj, ds, name = chartDSName, append = FALSE, force = TRUE)
            
            return(chart)
          })