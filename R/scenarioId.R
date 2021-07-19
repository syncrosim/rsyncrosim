# Copyright (c) 2021 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# GPL v.3 License
#' @include AAAClassDefinitions.R
NULL

#' Retrieves the scenarioId of a Scenario.
#'
#' Retrieves the scenarioId of a \code{\link{Scenario}}.
#'
#' @param scenario An object of class \code{\link{Scenario}}.
#' 
#' @return 
#' Integer id of the input scenario.
#' 
#' @examples 
#' \donttest{
#' temp_dir <- tempdir()
#' mySession <- session()
#' myLibrary <- ssimLibrary(name = file.path(temp_dir,"testlib"), session = mySession)
#' myProject <- project(myLibrary, project = "Definitions")
#' myScenario <- scenario(myProject, scenario = "My Scenario")
#' 
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
