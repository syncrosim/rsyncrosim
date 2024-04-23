# Copyright (c) 2024 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Add or remove values by column in a \code{\link{Chart}}
#'
#' Add or remove values by a specified column in the X or Y axis of a 
#' \code{\link{Chart}}.
#'
#' @param chart \code{\link{Chart}} object
#' @param axis character. Either "X" or "Y" corresponding to the X or Y axis of
#' the chart.
#' @param variable character. A variable belonging to the X or Y axis.
#' @param filter character or character vector. A filter column belonging to
#' the X or Y variable..
#' @param addValue character or character vector. Adds value(s) from the 
#' specified filter column and X or Y variable to be included in the chart.
#' @param removeValue character or character vector. Removes value(s) from the 
#' specified filter column and X or Y variable from being included in the chart.
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
#' # Include specific values in the chart
#' myChart <- chartInclude(myChart, variable = "variable1",
#' filter="col1", addValue=c("val1", "val2", "val3"))
#' 
#' # Remove specific values from the chart
#' myChart <- chartInclude(myChart, variable = "variable1",
#' filter="col1", removeValue="val3")
#' }
#' 
#' @export
setGeneric("chartInclude", function(chart, axis, variable, filter, addValue = NULL, 
                                    removeValue = NULL) standardGeneric("chartInclude"))

#' @rdname chartInclude
setMethod("chartInclude", signature(chart = "Chart"), 
          function(chart, axis, variable, filter, addValue, removeValue) {
            
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
    
    # Validate filter
    if (!is.character(filter) && (length(filter) == 1)){
      stop("filter must be a single character.")
    }
    
    if (axis == "X"){
      generalArgs <- append(list(`chart-include-x` = NULL), generalArgs)
    } else if (axis == "Y"){
      generalArgs <- append(list(`chart-include-y` = NULL), generalArgs)
    } else {
      stop("axis argument must be 'X' or 'Y'.")
    }
    
    # Include all values specified in addValue argument
    if (!is.null(addValue)){
      
      if (is.character(addValue)){
        
        for (v in addValue){
          
          # Convert v to ID values (can provide multiple IDs)
          
          args <- append(list(set = NULL, var = variable, col = filter), 
                         generalArgs)
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
