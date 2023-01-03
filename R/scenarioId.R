# Copyright (c) 2023 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Retrieves scenarioId of Scenario
#'
#' Retrieves the scenarioId of a \code{\link{Scenario}}.
#'
#' @param scenario \code{\link{Scenario}} object
#' 
#' @return 
#' Integer id of the input Scenario.
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
#' 
#' # Get Scenario ID of Scenario
#' scenarioId(myScenario)
#' }
#' 
#' @export
setGeneric("scenarioId", function(scenario) standardGeneric("scenarioId"))

#' @rdname scenarioId
setMethod("scenarioId", signature(scenario = "character"), function(scenario) {
  return(SyncroSimNotFound(scenario))
})

#' @rdname scenarioId
setMethod("scenarioId", signature(scenario = "Scenario"), function(scenario) {
  return(scenario@scenarioId)
})
