# Copyright (c) 2019 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# GPL v.3 License
#' @include AAAClassDefinitions.R
NULL

#' Retrieves the projectId of a SyncroSim Project or Scenario.
#'
#' Retrieves the projectId of a SyncroSim \code{\link{Project}} or \code{\link{Scenario}}.
#'
#' @param ssimObject An object of class \code{\link{Scenario}} or \code{\link{Project}}.
#' 
#' @return 
#' Returns an integer project id.
#' 
#' @examples 
#' \donttest{
#' temp_dir <- tempdir()
#' mySession <- session()
#' myLibrary <- ssimLibrary(name = file.path(temp_dir,"testlib"), session = mySession)
#' myProject <- project(myLibrary)
#' myScenario <- scenario(myProject)
#' 
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
