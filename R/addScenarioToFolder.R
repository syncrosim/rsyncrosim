# Copyright (c) 2023 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Add a Scenario to a Project folder
#'
#' Add a \code{\link{Scenario}} to a \code{\link{Project}} folder. 
#' Used to organize Scenarios in the SyncroSim User Interface.
#'
#' @param ssimObject \code{\link{Scenario}} object
#' @param folderId integer
#' 
#' @return 
#' Invisibly returns \code{TRUE} upon success (i.e.successful 
#' removal) and \code{FALSE} upon failure.
#' 
#' @examples
#' \donttest{
#' # Specify file path and name of new SsimLibrary
#' myLibraryName <- file.path(tempdir(), "testlib")
#' 
#' # Set up a SyncroSim Session and SsimLibrary
#' mySession <- session()
#' myLibrary <- ssimLibrary(name = myLibraryName, session = mySession)
#' myProject <- project(myLibrary, project = "Definitions")
#' 
#' # Create a new folder at the Project level and extract the folder Id
#' folderIdString <- createProjectFolder(myProject, "NewFolder")
#' folderId <- as.integer(strsplit(folderIdString, ": ")[[1]][2])
#' 
#' # Create Scenario and add to the above folder
#' myScenario <- scenario(myProject, "NewScenario")
#' addScenarioToFolder(myScenario, folderId)
#' }
#' 
#' @export
setGeneric("addScenarioToFolder", function(ssimObject, folderId) standardGeneric("addScenarioToFolder"))

#' @rdname addScenarioToFolder
setMethod("addScenarioToFolder", signature(ssimObject = "character"), function(ssimObject, folderId) {
  return(SyncroSimNotFound(ssimObject))
})

#' @rdname addScenarioToFolder
setMethod("addScenarioToFolder", signature(ssimObject = "Scenario"), function(ssimObject, folderId) {
  success <- FALSE
  args <- list(lib = .filepath(ssimObject), move = NULL, scenario = NULL, 
               sid = ssimObject@scenarioId, tfid = folderId, tpid = ssimObject@projectId)
  tt <- command(args = args, session = .session(ssimObject))
  
  if (tt == "saved"){
    success <- TRUE
  } 
  
  return(invisible(success))
})
