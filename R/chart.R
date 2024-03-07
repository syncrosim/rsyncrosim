# Copyright (c) 2024 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

setMethod(
  f = "initialize", signature = "Chart",
  definition = function(.Object, ssimObject, chart = "_Chart", type = "line", 
                        create = FALSE) {
    Name <- NULL
    ChartId <- NULL
    
    x <- ssimObject
    
    # Set some defaults
    ProjectId <- x@projectId

    # Grab all chart data
    charts <- getChartData(x)
    allCharts <- charts
    
    # Get chart name and validate
    if (is.character(chart)){
      charts <- subset(charts, Name == chart)
      
      # If more than 1 chart retrieved, then name is not unique
      if ((nrow(charts) > 1) & (create == FALSE)) {
        stop(paste0("folder provided is not unique. Either set create=TRUE to",
                    " create another chart with the same name or specify a ",
                    "unique name."))
      }
      
      Name <- chart
      
    } else if (is.numeric(chart)){
      
      charts <- subset(charts, ChartId == chart)
      
      # If no folders retrieved, then ID does not yet exist
      if (nrow(charts) == 0){
        stop(paste0("The project does not contain the given chart ID ", 
                    chart,
                    ". Please provide a name for the new chart - ",
                    "the ID will be assigned automatically by SyncroSim."))
      }
      
      if (create == TRUE) {
        stop(paste0("Cannot create a new chart from a chart ID. Please provide",
                    " a name for the new folder and the ID will be assigned",
                    " automatically."))
      }
      
      Name <- charts$Name
      
    } else{
      stop("chart argument must be a character or integer.")
    }
    
    # If one chart retrieved, then open chart
    if ((nrow(charts) == 1) & (create == FALSE)) {
      .Object@chartId <- chart$ChartId
      .Object@session <- .session(x)
      .Object@filepath <- .filepath(x)
      .Object@projectId <- ProjectId
      return(.Object)
    }
      
    # If no charts retrieved, then create a new chart
    if (!is(x, "Project")){
      stop(paste0("Can only create a new chart if the ssimObject provided ",
                  "is a SyncroSim Project."))
    }
    
    args <- list(lib = .filepath(x), create = NULL, chart = NULL, 
                 name = Name, pid = ProjectId, type = type)
    tt <- command(args = args, session = .session(x))
    ChartId <- as.integer(strsplit(tt, ": ")[[1]][2])
    
    .Object@chartId <- ChartId
    .Object@session <- .session(x)
    .Object@filepath <- .filepath(x)
    .Object@projectId <- ProjectId
    return(.Object)
  }
)

#' Create or open a chart
#'
#' Create or open a \code{\link{Chart}} from a SyncroSim
#' \code{\link{Project}}.
#'
#' @param ssimObject \code{\link{Project}} or \code{\link{Scenario}} object
#' @param chart character or integer. If character, then will either open an
#' existing chart if \code{create=FALSE}, or will create a new chart with the 
#' given name if the chart does not exist yet or \code{create=TRUE}. 
#' If integer, will open the existing chart with the given chart ID (if the
#' ID exists). If no value is provided and \code{create=TRUE}, a new chart will
#' be created with the default naming convention (e.g. "_Chart1", "_Chart2")
#' @param type character. Can be "line" (Default) or "bar"
#' @param create logical. Whether to create a new chart if the chart name given
#' already exists in the SyncroSim library. If \code{FALSE} (Default), then will 
#' return the existing chart with the given name. If \code{TRUE}, then will
#' return a new chart with the same name as an existing chart (but different
#' chart ID)
#' @param summary logical. If \code{TRUE}, returns a summary of chart 
#' information as an R data.frame. If \code{FALSE} (Default), then returns 
#' a SyncroSim Chart object
#' 
#' @return 
#' A \code{Chart} object representing a SyncroSim chart
#' 
#' @examples
#' \donttest{
#' # Set the file path and name of the new SsimLibrary
#' myLibraryName <- file.path(tempdir(),"testlib")
#' 
#' # Set the SyncroSim Session, SsimLibrary, Project, and Scenario
#' mySession <- session()
#' myLibrary <- ssimLibrary(name = myLibraryName, session = mySession) 
#' myProject <- project(myLibrary, project = "My Project")
#' myScenario <- scenario(myProject, scenario = "My Scenario")
#' 
#' # Create a new chart
#' myChart <- chart(myProject, chart = "New Chart", type = "line")
#' 
#' @name chart
#' @export
chart <- function(ssimObject = NULL, chart = NULL, type = "line", create = FALSE, summary = FALSE) {
  if (is.character(ssimObject) && (ssimObject == SyncroSimNotFound(warn = FALSE))) {
    return(SyncroSimNotFound())
  }
  
  # if ssimObject is a library throw an error
  if (is.element(class(ssimObject), c("SsimLibrary"))) {
    stop("Cannot create a folder at the Library-level.")
  }
  
  # Return chart data if no chart argument is specified
  if ((is.null(chart) && create == FALSE) || summary == TRUE){
    charts <- getChartData(ssimObject)
    
    return(charts)
  }
  
  obj <- new("Chart", ssimObject, chart = chart, type = type, create = create)
  
  return(obj)
}
