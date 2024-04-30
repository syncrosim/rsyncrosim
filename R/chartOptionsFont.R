# Copyright (c) 2024 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Modifies the font settings for a \code{\link{Chart}}
#'
#' Modifies the font style and size of various \code{\link{Chart}} components.
#'
#' @param chart \code{\link{Chart}} object
#' @param titleFont character. Sets the font for the title of the 
#' chart axes (e.g., "Microsoft Sans Serif, "Times New Roman", "Arial Narrow"). 
#' Default is \code{NULL}.
#' @param titleStyle character. Sets the font style for the title. Values can be
#' "standard", "italic", "bold", or "bold/italic". Default is \code{NULL}. 
#' @param titleSize integer. Sets the font size for the title of the 
#' chart axes. Default is \code{NULL}.
#' @param panelFont character. Sets the font for the title of the chart
#' panels (e.g., "Microsoft Sans Serif, "Times New Roman", "Arial"). Default 
#' is \code{NULL}.
#' @param panelStyle character. Sets the font style for the chart panels. Values 
#' can be "standard", "italic", "bold", or "bold/italic". Default is \code{NULL}. 
#' @param panelSize integer. Sets the font size for the chart panels. Default is 
#' \code{NULL}.
#' @param axisFont character. Sets the font for the title of the chart
#' panel axes (e.g., "Microsoft Sans Serif, "Times New Roman", "Arial"). Default 
#' is \code{NULL}.
#' @param axisStyle character. Sets the font style for the chart panel axes. Values 
#' can be "standard", "italic", "bold", or "bold/italic". Default is \code{NULL}.
#' @param axisSize integer. Sets the font size for the chart panel axes. Default 
#' is \code{NULL}.
#' @param legendFont character. Sets the font for the title of the 
#' chart legend (e.g., "Microsoft Sans Serif, "Times New Roman", "Arial"). Default 
#' is \code{NULL}.
#' @param legendStyle character. Sets the font style for the chart legend. Values 
#' can be "standard", "italic", "bold", or "bold/italic". Default is \code{NULL}.
#' @param legendSize integer. Sets the font size for the chart legend. Default 
#' is \code{NULL}.
#' 
#' @return 
#' A \code{Chart} object representing a SyncroSim chart or, if no arguments 
#' other than the chart are provided, a data.frame of the current chart font 
#' settings.
#' 
#' @examples
#' \dontrun{
#' # Open a chart object
#' myChart <- chart(myProject, chart = "My Chart")
#' 
#' # Set the font for the chart panels
#' myChart <- chartOptionsFont(myChart, panelFont = "Microsoft Sans Serif", 
#'                             panelStyle = "bold/italic", panelSize = 8)
#' 
#' # Return a dataframe of the current font settings
#' myChart <- chartOptionsFont(myChart)
#' }
#' 
#' @export
setGeneric("chartOptionsFont", function(
    chart, titleFont = NULL, titleStyle = NULL, titleSize = NULL, 
    panelFont = NULL, panelStyle = NULL, panelSize = NULL, axisFont = NULL, 
    axisStyle = NULL, axisSize = NULL, legendFont = NULL, legendStyle = NULL,
    legendSize = NULL) standardGeneric("chartOptionsFont"))

#' @rdname chartOptionsFont
setMethod("chartOptionsFont", signature(chart = "Chart"), 
          function(chart, titleFont, titleStyle, titleSize, panelFont, 
                   panelStyle, panelSize, axisFont, axisStyle, axisSize, 
                   legendFont, legendStyle, legendSize) {
           
  # Grab project and chart ID from chart
  proj <- .project(chart)
  chartCID <- .chartId(chart)
  chartDSName <- "core_Chart"
  defaultFont <- "Microsoft Sans Serif"
  defaultSize <- 9.75
  defaultStyle <- 0
  
  # Load chart configuration datasheet
  ds <- .datasheet(proj, name = chartDSName, optional = T, 
                   returnInvisible = T, includeKey = T, verbose = F)
  
  # Set variable for checking if we should just return a dataframe of settings
  allNULL <- TRUE
  
  # Set title font
  if (!is.null(titleFont) || !is.null(titleStyle) || !is.null(titleSize)){
    
    allNULL <- FALSE
    if (is.null(titleFont)){
      titleFont <- defaultFont
    }
    if (is.null(titleSize)){
      titleSize <- defaultSize
    }
    if (is.null(titleStyle)){
      titleStyle <- defaultStyle
    } else {
      titleStyle <- assignStyleFromString(titleStyle)
    }
    
    titleMerged <- paste0(titleFont, "|", titleSize, "|", titleStyle)
    ds[ds$ChartId == chartCID,]$ChartTitleFont <- titleMerged
  } 

  # Set panel font
  if (!is.null(panelFont) || !is.null(panelStyle) || !is.null(panelSize)){
    
    allNULL <- FALSE
    if (is.null(panelFont)){
      panelFont <- defaultFont
    }
    if (is.null(panelSize)){
      panelSize <- defaultSize
    }
    if (is.null(panelStyle)){
      panelStyle <- defaultStyle
    } else {
      panelStyle <- assignStyleFromString(panelStyle)
    }
    
    panelMerged <- paste0(panelFont, "|", panelSize, "|", panelStyle)
    ds[ds$ChartId == chartCID,]$PanelTitleFont <- panelMerged
  } 
  
  # Set panel axes font
  if (!is.null(axisFont) || !is.null(axisStyle) || !is.null(axisSize)){
    
    allNULL <- FALSE
    if (is.null(axisFont)){
      axisFont <- defaultFont
    }
    if (is.null(axisSize)){
      axisSize <- defaultSize
    }
    if (is.null(axisStyle)){
      axisStyle <- defaultStyle
    } else {
      axisStyle <- assignStyleFromString(axisStyle)
    }
    
    axisMerged <- paste0(axisFont, "|", axisSize, "|", axisStyle)
    ds[ds$ChartId == chartCID,]$PanelAxisFont <- axisMerged
  }
  
  # Set legend font
  if (!is.null(legendFont) || !is.null(legendStyle) || !is.null(legendSize)){
    
    allNULL <- FALSE
    if (is.null(legendFont)){
      legendFont <- defaultFont
    }
    if (is.null(legendSize)){
      legendSize <- defaultSize
    }
    if (is.null(legendStyle)){
      legendStyle <- defaultStyle
    } else {
      legendStyle <- assignStyleFromString(legendStyle)
    }
    
    legendMerged <- paste0(legendFont, "|", legendSize, "|", legendStyle)
    ds[ds$ChartId == chartCID,]$ChartLegendFont <- legendMerged
  }
  
  if (allNULL){
    ds <- ds[ds$ChartId == chartCID,]
    titlePieces <- strsplit(ds$ChartTitleFont, "\\|")[[1]]
    panelPieces <- strsplit(ds$PanelTitleFont, "\\|")[[1]]
    axisPieces <- strsplit(ds$PanelAxisFont, "\\|")[[1]]
    legendPieces <- strsplit(ds$ChartLegendFont, "\\|")[[1]]
    fontSettings <- data.frame(
      titleFont = if (length(titlePieces) == 1) defaultFont else titlePieces[1],
      titleSize = if (length(titlePieces) == 1) defaultSize else titlePieces[2],
      titleStyle = if (length(titlePieces) == 1) defaultStyle else assignStringFromStyle(titlePieces[3]),
      panelFont = if (length(panelPieces) == 1) defaultFont else panelPieces[1],
      panelSize = if (length(panelPieces) == 1) defaultSize else panelPieces[2],
      panelStyle = if (length(panelPieces) == 1) defaultStyle else assignStringFromStyle(panelPieces[3]),
      axisFont = if (length(axisPieces) == 1) defaultFont else axisPieces[1],
      axisSize = if (length(axisPieces) == 1) defaultSize else axisPieces[2],
      axisStyle = if (length(axisPieces) == 1) defaultStyle else assignStringFromStyle(axisPieces[3]),
      legendFont = if (length(legendPieces) == 1) defaultFont else legendPieces[1],
      legendSize = if (length(legendPieces) == 1) defaultSize else legendPieces[2],
      legendStyle = if (length(legendPieces) == 1) defaultStyle else assignStringFromStyle(legendPieces[3]))
    
    return(fontSettings)
  }
  
  saveDatasheet(proj, ds, name = chartDSName, append = FALSE, force = TRUE)
  
  return(chart)
})

assignStyleFromString <- function(styleString){
  if (styleString == "standard"){
    return(0)
  } else if (styleString == "bold"){
    return(1)
  } else if (styleString == "italic"){
    return(2)
  } else if (styleString == "bold/italic"){
    return(3)
  } else {
    stop("style must be one of 'standard', 'bold', 'italic', or 'bold/italic'.")
  }
}

assignStringFromStyle <- function(styleCode){
  if (styleCode == 0){
    return("standard")
  } else if (styleCode == 1){
    return("bold")
  } else if (styleCode == 2){
    return("italic")
  } else if (styleCode == 3){
    return("bold/italic")
  } else {
    stop("style code must be integer value between 0 and 3.")
  }
}
