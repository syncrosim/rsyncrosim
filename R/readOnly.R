# Copyright (c) 2021 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Read-only status of a SsimLibrary, Project or Scenario
#'
#' Retrieves or sets whether or not a \code{\link{SsimLibrary}}, 
#' \code{\link{Project}} or \code{\link{Scenario}} is read-only.
#'
#' @param ssimObject \code{\link{Scenario}}, 
#' \code{\link{Project}}, or \code{\link{SsimLibrary}} object
#' @param value logical. If \code{TRUE} the SsimObject will be read-only. Default is 
#' \code{FALSE}
#' 
#' @return 
#' A logical: \code{TRUE} if the SsimObject is read-only and \code{FALSE}
#' otherwise.
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
#' # Retrieve the read-only status of a SsimObject
#' readOnly(myLibrary)
#' readOnly(myProject)
#' readOnly(myScenario)
#' 
#' # Set the read-only status of a SsimObject
#' readOnly(myScenario) <- TRUE
#' }
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

setGeneric("readOnly<-", function(ssimObject, value) standardGeneric("readOnly<-"))

#' @rdname readOnly
setReplaceMethod(
  f = "readOnly",
  signature = "character",
  definition = function(ssimObject, value) {
    return(ssimObject)
  }
)

#' @rdname readOnly
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
