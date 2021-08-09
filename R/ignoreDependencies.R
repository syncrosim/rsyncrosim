# Copyright (c) 2021 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Ignore dependencies for a Scenario
#'
#' Retrieves or sets the Datafeeds to ignore for a \code{\link{Scenario}}.
#'
#' @param ssimObject \code{\link{Scenario}} object
#' @param value character string of Datafeed names to be ignored, separated by
#' commas (optional)
#' 
#' @return 
#' A character string: Scenario Datafeeds that will be ignored.
#' 
#' @examples
#' \donttest{
#' # Specify file path and name of new SsimLibrary
#' myLibraryName <- file.path(tempdir(), "testlib")
#' 
#' # Set up a SyncroSim Session, SsimLibrary, Project, and Scenario
#' mySession <- session()
#' myLibrary <- ssimLibrary(name = myLibraryName, session = mySession)
#' myProject <- project(myLibrary, project = "Definitions")
#' myScenario <- scenario(myProject, scenario = "My Scenario")
#' 
#' # List the Datafeeds to ignore
#' ignoreDependencies(myScenario)
#' 
#' # Set Scenario Datafeeds to ignore
#' ignoreDependencies(myScenario) <- "stsim_RunControl,stsim_TransitionTarget"
#' }
#' 
#' @export
setGeneric("ignoreDependencies", function(ssimObject) standardGeneric("ignoreDependencies"))

#' @rdname ignoreDependencies
setMethod("ignoreDependencies", signature(ssimObject = "character"), function(ssimObject) {
  return(SyncroSimNotFound(ssimObject))
})

#' @rdname ignoreDependencies
setMethod("ignoreDependencies", signature(ssimObject = "Scenario"), function(ssimObject) {
  scnInfo <- scenario(ssimObject, summary = TRUE)
  return(scnInfo$ignoreDependencies)
})

setGeneric("ignoreDependencies<-", function(ssimObject, value) standardGeneric("ignoreDependencies<-"))

#' @rdname ignoreDependencies
setReplaceMethod(
  f = "ignoreDependencies",
  signature = "character",
  definition = function(ssimObject, value) {
    return(ssimObject)
  }
)

#' @rdname ignoreDependencies
setReplaceMethod(
  f = "ignoreDependencies",
  signature = "Scenario",
  definition = function(ssimObject, value) {
    enquoted = NULL
    if (!is.null(value) && value != ""){
      enquoted = paste0('"', value, '"')
    }
    args <- list(setprop = NULL, lib = .filepath(ssimObject), ignoredeps = enquoted, sid = .scenarioId(ssimObject))
    tt <- command(args, .session(ssimObject))
    if (!identical(tt, "saved")) {
      stop(tt)
    }
    return(ssimObject)
  }
)
