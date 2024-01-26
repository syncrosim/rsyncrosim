# Copyright (c) 2024 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
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
#' @param value integer of the folder ID to move the \code{\link{Scenario}} to. 
#' Only applicable if the ssimObject provided is a \code{\link{Scenario}}.
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
#' myLibrary <- ssimLibrary(name = myLibraryName, 
#'                          session = mySession, 
#'                          overwrite = TRUE) 
#' myProject <- project(myLibrary, project = "Definitions")
#' myScenario <- scenario(myProject, scenario = "My Scenario")
#' myFolder <- folder(myProject, "New Folder")
#' 
#' # Get Folder ID for SyncroSim Folder and Scenario
#' folderId(myFolder)
#' folderId(myScenario)
#' 
#' # Move the Scenario into the newly created folder
#' folderId(myScenario) <- folderId(myFolder)
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
  parentFolderId <- getParentFolderId(ssimObject, ssimObject@scenarioId, 
                                      item="Scenario")
  folderInfo <- getFolderData(ssimObject)
  if (!parentFolderId %in% folderInfo$FolderId){
    parentFolderId <- NA
  }
  return(parentFolderId)
})
#' @rdname folderId
#' @export
setGeneric("folderId<-", function(ssimObject, value) standardGeneric("folderId<-"))

#' @rdname folderId
setReplaceMethod(
  f = "folderId",
  signature = "Scenario",
  definition = function(ssimObject, value) {
    
    if (!is.numeric(value)){
      stop("Can only assign a numeric value as the Scenario folder ID")
    }
    
    # If value == 0, same as NULL
    if (value == 0){
      return(ssimObject)
    }
    
    args <- list(lib = .filepath(ssimObject), move = NULL, scenario = NULL, 
                 sid = ssimObject@scenarioId, tfid = value, tpid = ssimObject@projectId)
    tt <- command(args = args, session = .session(ssimObject))
    if (!identical(tt, "saved")) {
      stop(tt)
    }
    return(ssimObject)
  }
)
