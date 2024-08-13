# Copyright (c) 2024 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Sets the \code{\link{Chart}} type and axes
#'
#' Sets the \code{\link{Chart}} type and adds the variables to plot
#' in the line chart.
#'
#' @param chart \code{\link{Chart}} object
#' @param type character. Chart type. Can be "Line" (Default) or "Column".
#' @param addX character or character vector. X variable(s) to add to the chart. 
#' If \code{NULL} (Default), does not add any X variables. If no X variables
#' specified in chart, then will default to plotting timesteps on the X axis.
#' @param addY character or character vector. Y variable(s) to add to the chart.
#' If \code{NULL} (Default), does not add any Y variables.
#' @param removeX character or character vector. X variable(s) to remove from 
#' plot. If \code{NULL} (Default), then does not remove any X variables.
#' @param removeY character or character vector. Y variable(s) to remove from 
#' plot. If \code{NULL} (Default), then does not remove any Y variables.
#' @param timesteps integer vector. The range of timesteps to plot against 
#' If \code{NULL}, then uses SyncroSim defaults.
#' @param iterationType character. How to display multiple iterations in the 
#' chart. Can be "Mean" (Default), "Single", or "All".
#' @param iteration integer. If the `iterationType` is set to "Single", this argument
#' determines which iteration to display. Default is 1.
#' 
#' @return 
#' A \code{Chart} object representing a SyncroSim chart
#' 
#' @examples
#' \dontrun{
#' # Create a chart object
#' myChart <- chart(myProject, chart = "New Chart")
#' 
#' # Set the chart type and data
#' myChart <- chartData(myChart, y = c("variable1", "variable2"),
#' timesteps = c(0,10), iterationType = "single", iteration = 1)
#' 
#' }
#' 
#' @export
setGeneric("chartData", function(chart, type = "Line", addX = NULL, addY = NULL,
                                 removeX = NULL, removeY = NULL,
                                 timesteps = NULL, iterationType = "Mean",
                                 iteration = 1) standardGeneric("chartData"))

#' @rdname chartData
setMethod("chartData", signature(chart = "Chart"), 
          function(chart, type, addX, addY, removeX, removeY, timesteps, 
                   iterationType, iteration) {
  
  # Set arguments used throughout
  chartSession <- .session(chart)
  proj <- .project(chart)
  libPath <- .filepath(.ssimLibrary(chart))
  chartPID <- .projectId(chart)
  chartCID <- .chartId(chart)
  consoleExe <- "SyncroSim.VizConsole.exe"
  chartDSName <- "core_Chart"
  generalArgs <- list(lib = libPath, pid = chartPID, cid = chartCID)
  
  # Load chart configuration datasheet
  ds <- .datasheet(proj, name = chartDSName, optional = T, 
                   returnInvisible = T, includeKey = T, verbose = F)
  
  # Set chart type and save before adding any variables
  if (!type %in% c("Line", "Column")){
    stop("type must be one of 'Line' or 'Column'.")
  } else {
    ds[ds$ChartId == chartCID,]$ChartType <- paste0(type, " Chart")
    saveDatasheet(proj, ds, name = chartDSName, append = FALSE, force = TRUE)
  }
  
  # Add x variables
  if (!is.null(addX)){
    
    if (is.character(addX)){
      
      for (xVar in addX){
        args <- append(list(set = NULL, chart = NULL, `variable-x` = NULL, 
                            var = xVar), generalArgs)
        tt <- command(args, session = chartSession, program = consoleExe)
        
        if (tt[1] != "saved"){
          stop(paste("Failed to add Chart X Variable:", tt[1]))
        } 
      }
    } else {
      stop("x must be a character or vector of characters.")
    }
  }
  
  # Remove x variables
  if (!is.null(removeX)){
    
    if (is.character(removeX)){
      
      for (xVar in removeX){
        args <- append(list(clear = NULL, chart = NULL, 
                            `variable-x` = NULL, var = xVar), generalArgs)
        tt <- command(args, session = chartSession, program = consoleExe)
        
        if (tt[1] != "saved"){
          stop(paste("Failed to remove Chart X Variable:", tt[1]))
        } 
      }
    } else {
      stop("x must be a character or vector of characters.")
    }
  }
            
  # Add y variable
  if (!is.null(addY)){
    
    if (is.character(addY)){
      
      for (yVar in addY){
        args <- append(list(set = NULL, chart = NULL, 
                            `variable-y` = NULL, var = yVar), generalArgs)
        tt <- command(args, session = chartSession, program = consoleExe)
        
        if (tt[1] != "saved"){
          stop(paste("Failed to add Chart Y Variable:", tt[1]))
        } 
      }
    } else {
      stop("y must be a character or vector of characters.")
    }
  }
  
  # Remove y variable
  if (!is.null(removeY)){
    
    if (is.character(removeY)){
      
      for (yVar in removeY){
        args <- append(list(clear = NULL, chart = NULL, 
                            `variable-y` = NULL, var = yVar), generalArgs)
        tt <- command(args, session = chartSession, program = consoleExe)
        
        if (tt[1] != "saved"){
          stop(paste("Failed to remove Chart Y Variable:", tt[1]))
        } 
      }
    } else {
      stop("y must be a character or vector of characters.")
    }
  }
            
  # Set timesteps
  if (!is.null(timesteps)){
    
    if (is.numeric(timesteps)){
      tsVar <- as.character(timesteps)
    } else if (is.vector(timesteps) && all(sapply(timesteps, is.numeric))){
      timesteps <- sapply(timesteps, as.integer)
      timesteps <- sapply(timesteps, as.character)
      tsVar <- paste0(timesteps, collapse=",")
    } else{
      stop("timesteps must be a vector of integers")
    }
    
    dsRow <- ds[ds$ChartId == chartCID,]
    if (dsRow$ChartType == "Column Chart"){
      ds[ds$ChartId == chartCID,]$TimestepsColumn <- tsVar
    } else {
      ds[ds$ChartId == chartCID,]$TimestepsLine <- tsVar
    }
  }
            
  # Set iteration type
  if (!is.null(iterationType)){
    
    if (!iterationType %in% c("Mean", "Single", "All")){
      stop("iterationType must be one of 'Mean', 'Single', or 'All'.")
    }
    
    ds[ds$ChartId == chartCID,]$IterationType <- iterationType
  }  
  
  # Set iteration
  if (!is.null(iteration)){
    
    if (is.numeric(iteration)){
      itVar <- as.character(iteration)
    } else{
      stop("iteration must be a single integer.")
    }
    
    ds[ds$ChartId == chartCID,]$Iteration <- iteration
  }
  
  saveDatasheet(proj, ds, name = chartDSName, append = FALSE, force = TRUE)
  
  return(chart)
})
