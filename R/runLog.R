# Copyright (c) 2021 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# GPL v.3 License
#' @include AAAClassDefinitions.R
NULL

#' Retrieves the run log of a result Scenario
#'
#' Retrieves the run log of a result Scenario.
#'
#' @param scenario A \code{\link{Scenario}} object.
#' 
#' @return 
#' Returns a character string: the run log for a result scenario.
#' 
#' @examples 
#' \dontrun{
#' addPackage("helloworldEnhanced")
#' temp_dir <- tempdir()
#' mySession <- session()
#' myLibrary <- ssimLibrary(name = file.path(temp_dir,"testlib"),
#'                          session = mySession, package = "helloworldEnhanced",
#'                          template = "example-library")
#' myProject <- project(myLibrary, project = "Definitions")
#' myScenario <- scenario(myProject, scenario = "My Scenario")
#' 
#' resultScenario <- run(myScenario)
#' 
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
