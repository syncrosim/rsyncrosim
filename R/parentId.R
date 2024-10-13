# Copyright (c) 2024 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Retrieves the parent Scenario id or parent Folder id
#'
#' Retrieves the id of the parent of a SyncroSim results Scenario or a SyncroSim
#' Folder.
#'
#' @param child \code{\link{Scenario}} or \code{\link{Folder}} object
#' 
#' @return 
#' An integer id of the parent Scenario if input is a Scenario, or an integer 
#' id of the parent Folder if input is a Folder. If the input Scenario or Folder 
#' does not have a parent, the function returns \code{NA}
#' 
#' @examples 
#' \dontrun{
#' # Set the file path and name of an existing SsimLibrary
#' myLibraryName <- "MyLibrary.ssim"
#' 
#' # Set the SyncroSim Session, SsimLibrary, Project, and Scenario
#' mySession <- session()
#' myLibrary <- ssimLibrary(name = myLibraryName,
#'                          session = mySession)
#' myProject <- project(myLibrary, project = "Definitions")
#' myScenario <- scenario(myProject, scenario = "My Scenario")
#' 
#' # Run Scenario to generate results
#' resultScenario <- run(myScenario)
#' 
#' # Find the parent ID of the Scenario
#' parentId(resultScenario)
#' }
#' 
#' @export
setGeneric("parentId", function(child) standardGeneric("parentId"))

#' @rdname parentId
setMethod("parentId", signature(child = "character"), function(child) {
  return(SyncroSimNotFound(child))
})

#' @rdname parentId
setMethod("parentId", signature(child = "Scenario"), function(child) {
  if (child@parentId == 0) {
    return(NA)
  }
  return(child@parentId)
})

#' @rdname parentId
setMethod("parentId", signature(child = "Folder"), function(child) {
  if (child@parentId == 0){
    return(NA)
  }
  return(child@parentId)
})
