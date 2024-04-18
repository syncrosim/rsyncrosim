# Copyright (c) 2024 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Sets the \code{\link{Chart}} type to "Line"
#'
#' Sets the \code{\link{Chart}} type to "Line" and adds the variables to plot
#' in the line chart.
#'
#' @param chart \code{\link{Chart}} object
#' @param type character. Chart type. Can be "Line" (Default) or "Column".
#' @param x character or character vector. X variable(s) to plot. If \code{NULL} 
#' (Default), then will plot timesteps along the X axis.
#' @param y character or character vector. Y variable(s) to plot.
#' @param timesteps integer vector. The range of timesteps to plot against 
#' (Default is the minimum and maximum timesteps defined in the simulation).
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
setGeneric("chartData", function(chart, type = "Line", x = NULL, y = NULL, 
                                 timesteps = NULL, iterationType = "Mean",
                                 iteration = 1) standardGeneric("chartData"))

#' @rdname chartData
setMethod("chartData", signature(chart = "Chart"), 
          function(chart, type, x, y, timesteps, iterationType, iteration) {
            
  # Set arguments used throughout
  chartSession <- .session(chart)
  libPath <- .filepath(.ssimLibrary(chart))
  chartPID <- .projectId(chart)
  chartCID <- .chartId(chart)
  consoleExe <- "SyncroSim.VizConsole.exe"
  generalArgs <- list(lib = libPath, pid = chartPID, cid = chartCID)
  
  # Set chart type
  
  if (!type %in% c("Line", "Column")){
    stop("type must be one of 'Line' or 'Column'.")
  }
  
  args <- append(list(set = NULL, `chart-type` = NULL, type = type), generalArgs)
  tt <- command(args, session = chartSession, program = consoleExe)
  
  if (tt[1] != "saved"){
    stop(paste("Failed to set Chart Type:", tt[1]))
  }
  
  # Set x variable
  if (!is.null(x) && (x != "Timesteps")){
    
    if (is.character(x)){
      
      # Clear existing variables first
      args <- append(list(clear = NULL, `chart-variable-x` = NULL), generalArgs)
      tt <- command(args, session = chartSession, program = consoleExe)
      
      for (xVar in x){
        args <- append(list(set = NULL, `chart-variable-x` = NULL, var = xVar), generalArgs)
        tt <- command(args, session = chartSession, program = consoleExe)
        
        if (tt[1] != "saved"){
          stop(paste("Failed to set Chart X Variable:", tt[1]))
        } 
      }
    } else {
      stop("x must be a character or vector of characters.")
    }
  }
            
  # Set y variable
  if (!is.null(y)){
    
    if (is.character(y)){
      
      # Clear existing variables first
      args <- append(list(clear = NULL, `chart-variable-y` = NULL), generalArgs)
      tt <- command(args, session = chartSession, program = consoleExe)
      
      for (yVar in y){
        args <- append(list(set = NULL, `chart-variable-y` = NULL, var = yVar), generalArgs)
        tt <- command(args, session = chartSession, program = consoleExe)
        
        if (tt[1] != "saved"){
          stop(paste("Failed to set Chart Y Variable:", tt[1]))
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
    
    args <- append(list(set = NULL, `chart-timesteps` = NULL, timesteps = tsVar), generalArgs)
    tt <- command(args, session = chartSession, program = consoleExe)
    
    if (tt[1] != "saved"){
      stop(paste("Failed to set Chart Timesteps:", tt[1]))
    }
  }
            
  # Set iteration type
  if (!is.null(iterationType)){
    
    if (!iterationType %in% c("Mean", "Single", "All")){
      stop("iterationType must be one of 'Mean', 'Single', or 'All'.")
    }
    
    args <- append(list(set = NULL, `chart-iter-type` = NULL, `iter-type` = iterationType), generalArgs)
    tt <- command(args, session = chartSession, program = consoleExe)
    
    if (tt[1] != "saved"){
      stop(paste("Failed to set Chart Iteration Type.", tt[1]))
    }
  }  
  
  # Set iteration
  if (!is.null(iteration)){
    
    if (is.numeric(iteration)){
      itVar <- as.character(iteration)
    } else{
      stop("iteration must be a single integer.")
    }
    
    args <- append(list(set = NULL, `chart-iteration` = NULL, iter = itVar), generalArgs)
    tt <- command(args, session = chartSession, program = consoleExe)
    
    if (tt[1] != "saved"){
      stop(paste("Failed to set Chart Iteration:", tt[1]))
    }
  }
})
