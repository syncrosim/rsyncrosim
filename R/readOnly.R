# Copyright (c) 2019 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# GPL v.3 License
#' @include AAAClassDefinitions.R
NULL

#' Read-only status of a SsimLibrary, Project or Scenario.
#'
#' Whether or not a \code{\link{SsimLibrary}}, \code{\link{Project}} or \code{\link{Scenario}} is read-only.
#'
#' @param ssimObject SsimLibrary, Project or Scenario.
#' 
#' @return 
#' Returns a logical value: `TRUE` if the ssimObject is read only and `FALSE`
#' otherwise.
#' 
#' @export
setGeneric("readOnly", function(ssimObject) standardGeneric("readOnly"))

#' @rdname readOnly
setMethod("readOnly", signature(ssimObject = "character"), function(ssimObject) {
  return(SyncroSimNotFound(ssimObject))
})

#' @rdname readOnly
setMethod("readOnly", signature(ssimObject = "SsimLibrary"), function(ssimObject) {
  cInfo <- info(ssimObject)
  property <- NULL
  oVal <- subset(cInfo, property == "Read Only:")$value
  rVal <- oVal
  if (oVal == "Yes") {
    rVal <- TRUE
  }
  if (oVal == "No") {
    rVal <- FALSE
  }
  return(rVal)
})

#' @rdname readOnly
setMethod("readOnly", signature(ssimObject = "Project"), function(ssimObject) {
  scnInfo <- project(ssimObject, summary = TRUE)
  return(scnInfo$readOnly)
})

#' @rdname readOnly
setMethod("readOnly", signature(ssimObject = "Scenario"), function(ssimObject) {
  scnInfo <- scenario(ssimObject, summary = TRUE)
  return(scnInfo$readOnly)
})

#' Set the read/write status of a SsimLibrary, Project or Scenario.
#'
#' Set the read-only status of a \code{\link{SsimLibrary}}, \code{\link{Project}} or \code{\link{Scenario}}.
#' Applies to child objects if ssimObject is a SsimLibrary or Project.
#'
#' @param ssimObject SsimLibrary, Project, or Scenario.
#' 
#' @param value Logical. If T the ssimObject will be read-only.
#' 
#' @return 
#' The updated ssimObject.
#' 
#' @export
setGeneric("readOnly<-", function(ssimObject, value) standardGeneric("readOnly<-"))

#' @rdname readOnly-set
setReplaceMethod(
  f = "readOnly",
  signature = "character",
  definition = function(ssimObject, value) {
    return(ssimObject)
  }
)

#' @rdname readOnly-set
setReplaceMethod(
  f = "readOnly",
  signature = "SsimObject",
  definition = function(ssimObject, value) {
    if (class(value) != "logical") {
      stop("readOnly must be TRUE or FALSE.")
    }
    if (value == TRUE) {
      readOnly <- "yes"
    } else {
      readOnly <- "no"
    }
    args <- list(setprop = NULL, lib = .filepath(ssimObject), readonly = readOnly)
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
