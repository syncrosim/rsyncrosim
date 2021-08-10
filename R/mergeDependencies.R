# Copyright (c) 2021 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Merge dependencies for a Scenario
#'
#' Retrieves or sets whether or not a \code{\link{Scenario}} is configured to 
#' merge dependencies at run time.
#'
#' @param ssimObject \code{\link{Scenario}} object
#' @param value logical. If \code{TRUE} the Scenario will be set to merge 
#' dependencies at run time. Default is \code{FALSE}
#' 
#' @return 
#' A logical: \code{TRUE} if the scenario is configured to merge dependencies at run time, 
#' and \code{FALSE} otherwise.
#' 
#' @examples 
#' \donttest{
#' # Specify file path and name of new SsimLibrary
#' myLibraryName <- file.path(tempdir(),"testlib")
#' 
#' # Set up a SyncroSim Session, SsimLibrary, Project, and Scenario
#' mySession <- session()
#' myLibrary <- ssimLibrary(name = myLibraryName, session = mySession)
#' myProject <- project(myLibrary, project = "Definitions")
#' myScenario <- scenario(myProject, scenario = "My Scenario")
#' 
#' # Retrieve whether or not dependencies will be merged for a Scenario
#' mergeDependencies(myScenario)
#' 
#' # Set whether or not dependencies will be merged for a Scenario
#' mergeDependencies(myScenario) <- TRUE
#' }
#' 
#' @export
setGeneric("mergeDependencies", function(ssimObject) standardGeneric("mergeDependencies"))

#' @rdname mergeDependencies
setMethod("mergeDependencies", signature(ssimObject = "character"), function(ssimObject) {
  return(SyncroSimNotFound(ssimObject))
})

#' @rdname mergeDependencies
setMethod("mergeDependencies", signature(ssimObject = "Scenario"), function(ssimObject) {
  scnInfo <- scenario(ssimObject, summary = TRUE)
  if (scnInfo$mergeDependencies == "Yes"){
    value <- TRUE
  } else if (scnInfo$mergeDependencies == "No"){
    value <- FALSE
  }
  return(value)
})

setGeneric("mergeDependencies<-", function(ssimObject, value) standardGeneric("mergeDependencies<-"))

#' @rdname mergeDependencies
setReplaceMethod(
  f = "mergeDependencies",
  signature = "character",
  definition = function(ssimObject, value) {
    return(ssimObject)
  }
)

#' @rdname mergeDependencies
setReplaceMethod(
  f = "mergeDependencies",
  signature = "Scenario",
  definition = function(ssimObject, value) {
    if (class(value) != "logical") {
      stop("mergeDependencies must be TRUE or FALSE.")
    }
    if (value == TRUE) {
      mergeDeps <- "yes"
    } else {
      mergeDeps <- "no"
    }
    args <- list(setprop = NULL, lib = .filepath(ssimObject), mergedeps = mergeDeps, sid = .scenarioId(ssimObject))
    tt <- command(args, .session(ssimObject))
    if (!identical(tt, "saved")) {
      stop(tt)
    }
    return(ssimObject)
  }
)
