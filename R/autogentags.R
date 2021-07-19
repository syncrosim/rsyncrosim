# Copyright (c) 2021 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# GPL v.3 License
#' @include AAAClassDefinitions.R
NULL

#' Auto Generation Tags for a Scenario.
#'
#' Retrieves the Auto Generation Tags for a \code{\link{Scenario}}.
#'
#' @param ssimObject An object of class \code{\link{Scenario}}.
#' 
#' @return 
#' Returns the Auto Generation Tags.
#' 
#' @examples  
#' \donttest {
#' temp_dir <- tempdir()
#' mySession <- session()
#' myLibrary <- ssimLibrary(name = file.path(temp_dir,"testlib"),
#'                          session = mySession)
#' myProject <- project(myLibrary, project = "Definitions")
#' myScenario <- scenario(myProject, scenario = "My Scenario")
#' 
#' autogentags(myScenario)
#' }
#' 
#' @export
setGeneric("autogentags", function(ssimObject) standardGeneric("autogentags"))

#' @rdname autogentags
setMethod("autogentags", signature(ssimObject = "character"), function(ssimObject) {
  return(SyncroSimNotFound(ssimObject))
})

#' @rdname autogentags
setMethod("autogentags", signature(ssimObject = "Scenario"), function(ssimObject) {
  scnInfo <- scenario(ssimObject, summary = TRUE)
  return(scnInfo$autoGenTags)
})

#' Auto Generation Tags for a Scenario.
#'
#' Sets the Auto Generation Tags for a \code{\link{Scenario}}.
#'
#' @param ssimObject An object of class \code{\link{Scenario}}.
#' @param value character.
#' 
#' @return 
#' The updated ssimObject.
#' 
#' @export
setGeneric("autogentags<-", function(ssimObject, value) standardGeneric("autogentags<-"))

#' @rdname autogentags-set
setReplaceMethod(
  f = "autogentags",
  signature = "character",
  definition = function(ssimObject, value) {
    return(ssimObject)
  }
)

#' @rdname autogentags-set
setReplaceMethod(
  f = "autogentags",
  signature = "Scenario",
  definition = function(ssimObject, value) {
    args <- list(setprop = NULL, lib = .filepath(ssimObject), autogentags = value, sid = .scenarioId(ssimObject))
    tt <- command(args, .session(ssimObject))
    if (!identical(tt, "saved")) {
      stop(tt)
    }
    return(ssimObject)
  }
)
