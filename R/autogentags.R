# Copyright (c) 2021 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Auto Generation Tags for a Scenario
#'
#' Retrieves or sets the Auto Generation Tags for a \code{\link{Scenario}}.
#'
#' @param ssimObject \code{\link{Scenario}} object
#' @param value character
#' 
#' @return 
#' Returns the Auto Generation Tags.
#' 
#' @examples  
#' \dontrun{
#' # Get the Auto Generation Tags for a SyncroSim Scenario
#' autogentags(myScenario)
#' 
#' # Set the Auto Generation Tags for a SyncroSim Scenario
#' autogentags(myScenario) <- "myTag"
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

#' @export
setGeneric("autogentags<-", function(ssimObject, value) standardGeneric("autogentags<-"))

#' @rdname autogentags
setReplaceMethod(
  f = "autogentags",
  signature = "character",
  definition = function(ssimObject, value) {
    return(ssimObject)
  }
)

#' @rdname autogentags
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
