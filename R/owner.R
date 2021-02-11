# Copyright (c) 2019 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# GPL v.3 License
#' @include AAAClassDefinitions.R
NULL

#' The owner of a SsimLibrary/Project/Scenario.
#'
#' Retrieves the owner of a SsimLibrary/ProjectScenario.
#'
#' @param ssimObject SsimLibrary/Project/Scenario.
#' 
#' @return 
#' A character string: the owner of the ssimObject. 
#' 
#' @export
setGeneric("owner", function(ssimObject) standardGeneric("owner"))

#' Set the owner of an SsimLibrary/Project/Scenario.
#'
#' Set the owner of an SsimLibrary/Project/Scenario.
#'
#' @param ssimObject Scenario/Project/SsimLibrary.
#' @param value The new owner.
#' 
#' @return 
#' The updated ssimObject.
#' 
#' @export
setGeneric("owner<-", function(ssimObject, value) standardGeneric("owner<-"))

#' @rdname owner
setMethod("owner", signature(ssimObject = "character"), function(ssimObject) {
  return(SyncroSimNotFound(ssimObject))
})

#' @rdname owner
setMethod("owner", signature(ssimObject = "SsimLibrary"), function(ssimObject) {
  cInfo <- info(ssimObject)
  property <- NULL
  return(subset(cInfo, property == "Owner:")$value)
})

#' @rdname owner
setMethod("owner", signature(ssimObject = "Project"), function(ssimObject) {
  scnInfo <- project(ssimObject, summary = TRUE)
  return(scnInfo$owner)
})

#' @rdname owner
setMethod("owner", signature(ssimObject = "Scenario"), function(ssimObject) {
  scnInfo <- scenario(ssimObject, summary = TRUE)
  return(scnInfo$owner)
})

#' @rdname owner-set
setReplaceMethod(
  f = "owner",
  signature = "character",
  definition = function(ssimObject, value) {
    return(ssimObject)
  }
)

#' @rdname owner-set
setReplaceMethod(
  f = "owner",
  signature = "SsimObject",
  definition = function(ssimObject, value) {
    args <- list(setprop = NULL, lib = .filepath(ssimObject), owner = value)
    if (class(ssimObject) == "Project") {
      args$pid <- .projectId(ssimObject)
    }
    if (class(ssimObject) == "Scenario") {
      args$sid <- .scenarioId(ssimObject)
    }
    tt <- command(args, .session(ssimObject))
    if (!identical(tt, "saved")) {
      stop(tt)
    }
    return(ssimObject)
  }
)
