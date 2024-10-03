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
#' @param variable character. The name of a charting variable. If provided,
#' then will return a list of the available filter columns for that variable.
#' Default is \code{NULL}.
#' @param filter character. The name of a filter column for a specified 
#' variable. If provided, then will return a list of values that pertain to 
#' the specified filter. If the filter column is used to disaggregate the 
#' chart data (using the \code{\link{chartDisagg}} function), one panel will be 
#' created for each of these values. If you would like to omit values from the 
#' chart, you can also add or remove values by the specified filter column 
#' using the \code{\link{chartInclude}} function. Default is \code{NULL}.
#' 
#' @return 
#' A data.frame or list of variables, filter columns, and filter values.
#' 
#' @details  
#' Example arguments:
#' \itemize{
#'   \item If ssimObject is SyncroSim Project and chart is \code{NULL}: Returns
#'          a data.frame of available variables for creating a new chart.
#'   \item If ssimObject is SyncroSIm Chart or chart is not \code{NULL}: Returns
#'          a data.frame of variables in use by the specified chart.
#'   \item If variable is not \code{NULL}: Returns a list of filter columns
#'          that belong to the given variable.
#'   \item If variable and filter are not \code{NULL}: Returns a dataframe of 
#'         value IDs and names that belong to the given variable and filter.
#' }
#' 
#' @examples
#' \dontrun{
#' # Create a chart object
#' myChart <- chart(myProject, chart = "New Chart")
#' 
#' # Retrieve variables that can be used to create new charts
#' chartCriteria(myProject)
#' 
#' # Retrieve variables being used by existing chart
#' chartCriteria(myChart)
#' }
#' 
#' @export
setGeneric("chartCriteria", function(ssimObject, chart = NULL, variable = NULL, 
                                 filter = NULL) standardGeneric("chartCriteria"))

#' @rdname chartCriteria
setMethod("chartCriteria", signature(ssimObject = "SsimObject"), 
          function(ssimObject, chart, variable, filter) {

    # Set arguments used throughout
    chartSession <- .session(ssimObject)
    libPath <- .filepath(.ssimLibrary(ssimObject))
    chartPID <- .projectId(ssimObject)
    consoleExe <- "SyncroSim.VizConsole.exe"
    
    # Validate variable and filter
    if (is.null(variable) && !is.null(filter)){
      stop("Cannot return filter information without a variable.")
    }
    
    returnSingleChart <- (is(ssimObject, "Chart") || !is.null(chart)) && 
      (is.null(variable))
    returnAllChart <- (is(ssimObject, "Project") || !is.null(variable))
            
    if (returnSingleChart){
      
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
      
      df <- data.frame()
      
      args <- list(list = NULL, chart = NULL, `criteria-y` = NULL, lib = libPath, 
                   pid = chartPID, cid = chartCID, csv = NULL)
      yResult <- command(args, session = chartSession, program = consoleExe)
      yResult <- .dataframeFromSSim(yResult)
      
      if (nrow(yResult) > 0){
        yResult$axis <- "y"
        df <- yResult
      }
      
      args <- list(list = NULL, chart = NULL, `criteria-x` = NULL, lib = libPath, 
                   pid = chartPID, cid = chartCID, csv = NULL)
      xResult <- command(args, session = chartSession, program = consoleExe)
      xResult <- .dataframeFromSSim(xResult)
      
      if (nrow(xResult) > 0){
        xResult$axis <- "x"
        missingCol <- setdiff(names(yResult), names(xResult))
        xResult[missingCol] <- "N/A" 
        df <- rbind(df, xResult)
      }
      
      # Grab list of chart information for project to match names
      if (is(ssimObject, "Project")){
        ssimProj <- ssimObject
      } else {
        ssimProj <- .project(ssimObject)
      }
      
      # dissagregateBy should match filter values in projectchartCriteria
      for (i in 1:nrow(df)){
        
        if (nrow(df) == 0){
          break
        }
        
        dfRow <- df[i,]
        availableFilters <- chartCriteria(ssimProj, variable = dfRow$variable)
        chartFilters <- strsplit(dfRow$disaggregateBy, split = "|", 
                                 fixed = TRUE)[[1]]
        finalFilters <- c()
        
        for (cf in chartFilters) {
          for (af in availableFilters) {
            if (tolower(cf) == tolower(af)) {
              finalFilters <- c(finalFilters, af)
            }
          }
        }
        
        if (length(finalFilters) == 0){
          df[i, "disaggregateBy"] <- "N/A"
        } else {
          df[i, "disaggregateBy"] <- paste(finalFilters, collapse = '|')
        }
          
        chartIncludeData <- strsplit(dfRow$includeDataFor, split = "|", 
                                     fixed = TRUE)[[1]]
        finalIncludeData <- c()
        
        for (cid in chartIncludeData) {
          for (af in availableFilters) {
            includeData <- strsplit(cid, split = "=", fixed = TRUE)[[1]]
            includeDataFilter <- includeData[1]
            includeDataValue <- includeData[2]
            
            if (tolower(includeDataFilter) == tolower(af)) {
              newIncludeDataString <- paste0(af, "=", includeDataValue)
              finalIncludeData <- c(finalIncludeData, newIncludeDataString)
            }
          }
        }
        
        if (length(finalIncludeData) == 0){
          df[i, "includeDataFor"] <- "N/A"
        } else {
          df[i, "includeDataFor"] <- paste(finalIncludeData, collapse = '|')
        }
      }
    }
    
    else if (returnAllChart){
      args <- list(list = NULL, chart = NULL, `variables` = NULL, lib = libPath, 
                   pid = chartPID, csv = NULL)
      df <- command(args, session = chartSession, program = consoleExe)
      df <- .dataframeFromSSim(df)
      
      if (!is.null(variable)){
        subsetInfo <- df[df$name == variable,]
        
        if (is.null(filter)){
          
          filters <- strsplit(subsetInfo$filters, split = "|", fixed = TRUE)[[1]]
          return(filters) 
          
        } else {

          chartDS <- subsetInfo$datasheet
          args <- list(list = NULL, columns = NULL, lib=libPath, allprops = NULL, 
                       sheet = chartDS, csv = NULL)
          tt <- command(args, mySession)
          df <- .dataframeFromSSim(tt)
          df_filtered <- df[df$name %in% filter, ]
          
          if (nrow(df_filtered) == 0){
            stop(paste("filter argument", filter, "does not exist for variable", variable))
          }
          
          displayNameDS <- gsub(".*formula1\\^(.+)\\!formula2.*", "\\1", df_filtered$properties)
          valuesDS <- .datasheet(myProject, name = displayNameDS, rawValues = T, includeKey = T)
          names(valuesDS) <- c("ID", "Name")
          return(valuesDS[1:2])
        }
      }
    }
    
    else {
      stop("ssimObject must be a SyncroSim Project or SyncroSim Chart object.")
    }
    
    return(df)
  })
