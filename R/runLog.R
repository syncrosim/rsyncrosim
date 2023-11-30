# Copyright (c) 2023 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Retrieves run log of result Scenario
#'
#' Retrieves the run log of a result Scenario.
#'
#' @param scenario \code{\link{Scenario}} object.
#' 
#' @return 
#' A character string: the run log for a result scenario.
#' 
#' @examples 
#' \dontrun{
#' # Install helloworldSpatial package
#' installPackage("helloworldSpatial")
#' 
#' # Set the file path and name of the new SsimLibrary
#' myLibraryName <- file.path(tempdir(),"testlib")
#' 
#' # Set the SyncroSim Session, SsimLibrary, Project, and Scenario
#' mySession <- session()
#' myLibrary <- ssimLibrary(name = myLibraryName,
#'                          session = mySession, 
#'                          package = "helloworldSpatial",
#'                          template = "example-library",
#'                          forceUpdate = TRUE)
#' myProject <- project(myLibrary, project = "Definitions")
#' myScenario <- scenario(myProject, scenario = "My Scenario")
#' 
#' # Run Scenario
#' resultScenario <- run(myScenario)
#' 
#' # Retrieve the run log of the result Scenario
#' runLog(resultScenario)
#' }
#' 
#' @export
setGeneric("runLog", function(scenario) standardGeneric("runLog"))

#' @rdname runLog
setMethod("runLog", signature(scenario = "character"), function(scenario) {
  return(SyncroSimNotFound(scenario))
})

#' @rdname runLog
setMethod("runLog", signature(scenario = "Scenario"), function(scenario) {
  tt <- command(list(list = NULL, runlog = NULL, lib = .filepath(scenario), sid = .scenarioId(scenario)), .session(scenario))
  if (grepl("The scenario is not a result scenario", tt[1], fixed = TRUE)) {
    tt <- tt[1]
    return(tt)
  }

  outString <- paste(tt, collapse = "\n")
  writeLines(outString)
  return(outString)
})
