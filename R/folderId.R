# Copyright (c) 2023 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Retrieves folderId of SyncroSim Folder or Scenario
#'
#' Retrieves the Folder Id of a SyncroSim \code{\link{Folder}} or 
#' \code{\link{Scenario}}. Can also use to set the Folder Id for a 
#' \code{\link{Scenario}} - this will move the \code{\link{Scenario}} into the
#' desired folder in the SyncroSim User Interface.
#'
#' @param ssimObject \code{\link{Folder}} or \code{\link{Scenario}} object
#' 
#' @return 
#' An integer: folder id.
#' 
#' @examples 
#' \donttest{
#' # Set the file path and name of the new SsimLibrary
#' myLibraryName <- file.path(tempdir(),"testlib")
#' 
#' # Set the SyncroSim Session, SsimLibrary, Project, and Scenario
#' mySession <- session()
#' myLibrary <- ssimLibrary(name = myLibraryName, session = mySession) 
#' myProject <- project(myLibrary, project = "Definitions")
#' myScenario <- scenario(myProject, scenario = "My Scenario")
#' myFolder <- folder(myProject, "New Folder")
#' 
#' # Get Folder ID for SyncroSim Folder and Scenario
#' folderId(myFolder)
#' folderId(myScenario)
#' }
#' 
#' @export
setGeneric("folderId", function(ssimObject) standardGeneric("folderId"))
#' @rdname folderId
setMethod("folderId", signature(ssimObject = "character"), function(ssimObject) {
  return(SyncroSimNotFound(ssimObject))
})
#' @rdname folderId
setMethod("folderId", signature(ssimObject = "Folder"), function(ssimObject) {
  return(ssimObject@folderId)
})
#' @rdname folderId
setMethod("folderId", signature(ssimObject = "Scenario"), function(ssimObject) {
  return(ssimObject@folderId)
})
