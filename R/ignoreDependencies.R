# Copyright (c) 2021 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# GPL v.3 License
#' @include AAAClassDefinitions.R
NULL

#' Ignore Dependencies for a Scenario.
#'
#' Retrieves the Ignore Dependencies Datafeeds for a \code{\link{Scenario}}.
#'
#' @param ssimObject An object of class \code{\link{Scenario}}.
#' 
#' @return 
#' Returns Ignore Dependencies Datafeeds.
#' 
#' @examples
#' \donttest{
#' temp_dir <- tempdir()
#' mySession <- session()
#' myLibrary <- ssimLibrary(name = file.path(temp_dir,"testlib"), session = mySession)
#' myProject <- project(myLibrary, project = "Definitions")
#' myScenario <- scenario(myProject, scenario = "My Scenario")
#' 
#' # Lists the datafeeds to ignore
#' ignoreDependencies(myScenario)
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

#' Ignore Dependencies Datafeeds for a Scenario.
#'
#' Sets the Ignore Dependencies Datafeed for a \code{\link{Scenario}}.
#'
#' @param ssimObject An object of class \code{\link{Scenario}}.
#' @param value The datafeed names to be ignored, separated by commas.
#' 
#' @return 
#' Returns the updated ssimObject.
#' 
#' @examples
#' \donttest{
#' temp_dir <- tempdir()
#' mySession <- session()
#' myLibrary <- ssimLibrary(name = file.path(temp_dir,"testlib"), session = mySession)
#' myProject <- project(myLibrary, project = "Definitions")
#' myScenario <- scenario(myProject, scenario = "My Scenario")
#' 
#' # IgnoreDependencies is a string of datafeed names separated by commas
#' ignoreDependencies(myScenario) <- "stsim_RunControl,stsim_TransitionTarget"
#' }
#' 
#' @export
setGeneric("ignoreDependencies<-", function(ssimObject, value) standardGeneric("ignoreDependencies<-"))

#' @rdname ignoreDependencies-set
setReplaceMethod(
  f = "ignoreDependencies",
  signature = "character",
  definition = function(ssimObject, value) {
    return(ssimObject)
  }
)

#' @rdname ignoreDependencies-set
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
