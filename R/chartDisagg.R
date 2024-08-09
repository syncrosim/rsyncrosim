# Copyright (c) 2024 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Disaggregates the \code{\link{Chart}} by a Y variable
#'
#' Disaggregates the \code{\link{Chart}} by given filter column(s) in a Y 
#' variable.
#'
#' @param chart \code{\link{Chart}} object
#' @param variable character. The variable to disaggregate the Y axis by.
#' @param addFilter character or character vector. Adds Y variable column(s) to 
#' disaggregate the chart by.
#' @param removeFilter character or character vector. Removes Y variable 
#' column(s) from disaggregating the chart.
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
#' # Disaggregate the chart by a filter column
#' myChart <- chartDisagg(myChart, variable = "variable1",
#' addFilter=c("col1", "col2"))
#' 
#' # Remove a filter column from the chart disaggregation
#' myChart <- chartDisagg(myChart, variable = "variable1",
#' removeFilter="col1")
#' }
#' 
#' @export
setGeneric("chartDisagg", function(chart, variable, addFilter = NULL, 
                                   removeFilter = NULL) standardGeneric("chartDisagg"))

#' @rdname chartDisagg
setMethod("chartDisagg", signature(chart = "Chart"), 
          function(chart, variable, addFilter, removeFilter) {
            
    # Set arguments used throughout
    chartSession <- .session(chart)
    libPath <- .filepath(.ssimLibrary(chart))
    chartCID <- .chartId(chart)
    consoleExe <- "SyncroSim.VizConsole.exe"
    generalArgs <- list(lib = libPath, cid = chartCID)
    
    # Validate variable
    if (!is.character(variable) && (length(variable) == 1)){
      stop("variable must be a single character.")
    }
    
    # Disaggregate Y variable by each filter column provided
    if (!is.null(addFilter)){
      
      if (is.character(addFilter)){
        
        for (filterCol in addFilter){
          args <- append(list(set = NULL, `chart-disagg-y` = NULL, 
                              var = variable, col = filterCol), generalArgs)
          tt <- command(args, session = chartSession, program = consoleExe)
          
          if (tt[1] != "saved"){
            stop(paste("Failed to disaggregate by column", filterCol, ":", tt[1]))
          } 
        }
      } else {
        stop("addFilter must be a character or vector of characters.")
      }
    }
    
    # Remove Y variable disaggregations by filter column provided
    if (!is.null(removeFilter)){
      
      if (is.character(removeFilter)){
        
        for (filterCol in removeFilter){
          args <- append(list(clear = NULL, `chart-disagg-y` = NULL, 
                              var = variable, col = filterCol), generalArgs)
          tt <- command(args, session = chartSession, program = consoleExe)
          
          if (tt[1] != "saved"){
            stop(paste("Failed to remove disaggregate by column", filterCol, 
                       ":", tt[1]))
          } 
        }
      } else {
        stop("addFilter must be a character or vector of characters.")
      }
    }
    
    return(chart)
  })
