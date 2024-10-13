# Copyright (c) 2024 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Retrieves projectId of SyncroSim Project, Scenario, Folder, or Chart
#'
#' Retrieves the projectId of a SyncroSim \code{\link{Project}},
#' \code{\link{Scenario}}, \code{\link{Folder}} or \code{\link{Chart}}.
#'
#' @param ssimObject \code{\link{Scenario}}, \code{\link{Project}}, 
#' \code{\link{Folder}}, or \code{\link{{Chart}}} object
#' 
#' @return 
#' An integer: project id.
#' 
#' @examples 
#' \dontrun{
#' # Set the file path and name of the new SsimLibrary
#' myLibraryName <- file.path(tempdir(),"testlib")
#' 
#' # Set the SyncroSim Session, SsimLibrary, Project, and Scenario
#' mySession <- session()
#' myLibrary <- ssimLibrary(name = myLibraryName, session = mySession) 
#' myProject <- project(myLibrary, project = "Definitions")
#' myScenario <- scenario(myProject, scenario = "My Scenario")
#' 
#' # Get Project ID for SyncroSim Project and Scenario
#' projectId(myProject)
#' projectId(myScenario)
#' }
#' 
#' @export
setGeneric("projectId", function(ssimObject) standardGeneric("projectId"))
#' @rdname projectId
setMethod("projectId", signature(ssimObject = "character"), function(ssimObject) {
  return(SyncroSimNotFound(ssimObject))
})
#' @rdname projectId
setMethod("projectId", signature(ssimObject = "Project"), function(ssimObject) {
  return(ssimObject@projectId)
})
#' @rdname projectId
setMethod("projectId", signature(ssimObject = "Scenario"), function(ssimObject) {
  return(ssimObject@projectId)
})
#' @rdname projectId
setMethod("projectId", signature(ssimObject = "Folder"), function(ssimObject) {
  return(ssimObject@projectId)
})
#' @rdname projectId
setMethod("projectId", signature(ssimObject = "Chart"), function(ssimObject) {
  return(ssimObject@projectId)
})
