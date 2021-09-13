# Copyright (c) 2021 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Retrieves the parent Scenario id
#'
#' Retrieves the id of the parent of a SyncroSim results Scenario.
#'
#' @param scenario \code{\link{Scenario}} object
#' 
#' @return 
#' An integer id of the parent Scenario. If the input Scenario does not have a
#' parent, the function returns \code{NA}
#' 
#' @examples 
#' \donttest{
#' # Install helloworldEnhanced SyncroSim package
#' addPackage("helloworldEnhanced")
#' 
#' # Set the file path and name of the new SsimLibrary
#' myLibraryName <- file.path(tempdir(),"testlib_parentId")
#' 
#' # Set the SyncroSim Session, SsimLibrary, Project, and Scenario
#' mySession <- session()
#' myLibrary <- ssimLibrary(name = myLibraryName,
#'                          session = mySession,
#'                          package = "helloworldEnhanced",
#'                          template = "example-library")
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
setGeneric("parentId", function(scenario) standardGeneric("parentId"))

#' @rdname parentId
setMethod("parentId", signature(scenario = "character"), function(scenario) {
  return(SyncroSimNotFound(scenario))
})

#' @rdname parentId
setMethod("parentId", signature(scenario = "Scenario"), function(scenario) {
  if (scenario@parentId == 0) {
    return(NA)
  }
  return(scenario@parentId)
})
