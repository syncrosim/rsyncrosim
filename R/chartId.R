# Copyright (c) 2024 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Retrieves chartId of SyncroSim Chart
#'
#' Retrieves the Chart Id of a SyncroSim \code{\linkS4class{Chart}}.
#'
#' @param ssimObject \code{\linkS4class{Chart}} object
#' 
#' @return 
#' An integer: chart id.
#' 
#' @examples 
#' \donttest{
#' # Set the file path and name of the new SsimLibrary
#' myLibraryName <- file.path(tempdir(), "testlib")
#' 
#' # Set the SyncroSim Session, SsimLibrary, and Project
#' mySession <- session()
#' myLibrary <- ssimLibrary(name = myLibraryName, 
#'                          session = mySession, 
#'                          packages = "stsim",
#'                          overwrite = TRUE) 
#' myProject <- project(myLibrary, project = "Definitions")
#' 
#' # Get the chart object corresponding to the chart called "My Chart"
#' myChart <- chart(myProject, chart = "My Chart")
#' 
#' # Get Chart ID for SyncroSim Chart
#' chartId(myChart)
#' }
#' 
#' @export
setGeneric("chartId", function(ssimObject) standardGeneric("chartId"))

#' @rdname chartId
setMethod("chartId", signature(ssimObject = "character"), function(ssimObject) {
  return(SyncroSimNotFound(ssimObject))
})

#' @rdname chartId
setMethod("chartId", signature(ssimObject = "Chart"), function(ssimObject) {
  return(ssimObject@chartId)
})