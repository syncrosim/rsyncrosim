# Copyright (c) 2024 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Retrieves chart variables
#'
#' Retrieves the available variables for charting, or the variables that are 
#' set for an existing chart.
#'
#' @param ssimObject \code{\link{Project}} or \code{\link{Chart}} object
#' @param chart character or integer. Either the name or ID of an existing chart.
#' If \code{NULL} and a \code{\link{Project}} is provided as the first argument,
#' then will return the available variables for charting.
#' 
#' @return 
#' A data.frame of variables, filter columns, and filter values.
#' 
#' @details  
#' Example arguments:
#' \itemize{
#'   \item If ssimObject is SyncroSim Project and chart is \code{NULL}: Returns
#'          a data.frame of available variables for creating a new chart.
#'   \item If ssimObject is SyncroSIm Chart or chart is not \code{NULL}: Returns
#'          a data.frame of variables in use by the specified chart.
#' }
#' 
#' @examples
#' \dontrun{
#' # Create a chart object
#' myChart <- chart(myProject, chart = "New Chart")
#' 
#' # Retrieve variables that can be used to create new charts
#' chartInfo(myProject)
#' 
#' # Retrieve variables being used by existing chart
#' chartInfo(myChart)
#' }
#' 
#' @export
setGeneric("chartInfo", function(ssimObject, chart = NULL) standardGeneric("chartInfo"))

#' @rdname chartInfo
setMethod("chartInfo", signature(ssimObject = "SsimObject"), 
          function(ssimObject, chart) {

    # Set arguments used throughout
    chartSession <- .session(ssimObject)
    libPath <- .filepath(.ssimLibrary(ssimObject))
    chartPID <- .projectId(ssimObject)
    consoleExe <- "SyncroSim.VizConsole.exe"
            
    if (is(ssimObject, "Chart") || !is.null(chart)){
      
      if (is(ssimObject, "Chart")){
        chartCID <- .chartId(ssimObject)
      } else if (is.numeric(chart)) {
        chartCID <- chart
      } else if (is.character(chart)) {
        chartObject <- .chart(ssimObject, chart)
        chartCID <- .chartId(chartObject)
      } else {
        stop("chart argument must be a character or integer.")
      }
      
      args <- list(list = NULL, `chart-criteria-y` = NULL, lib = libPath, 
                   pid = chartPID, cid = chartCID, csv = NULL)
      yResult <- command(args, session = chartSession, program = consoleExe)
      yResult <- .dataframeFromSSim(yResult)
      yResult$axis <- "y"
      
      args <- list(list = NULL, `chart-criteria-x` = NULL, lib = libPath, 
                   pid = chartPID, cid = chartCID, csv = NULL)
      xResult <- command(args, session = chartSession, program = consoleExe)
      xResult <- .dataframeFromSSim(xResult)
      xResult$axis <- "x"
      missingCol <- setdiff(names(yResult), names(xResult))
      xResult[missingCol] <- "N/A"
      
      df <- rbind(yResult, xResult)
    }
    
    else if (is(ssimObject, "Project")){
      args <- list(list = NULL, `chart-variables` = NULL, lib = libPath, 
                   pid = chartPID, csv = NULL)
      df <- command(args, session = chartSession, program = consoleExe)
      df <- .dataframeFromSSim(df)
    }
    
    else {
      stop("ssimObject must be a SyncroSim Project or SyncroSim Chart object.")
    }
    
    return(df)
  })
