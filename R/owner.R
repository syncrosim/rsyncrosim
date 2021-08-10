# Copyright (c) 2021 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Owner of a SsimLibrary, Project or Scenario
#'
#' Retrieves or sets the owner of a \code{\link{SsimLibrary}},
#' \code{\link{Project}} or \code{\link{Scenario}}.
#'
#' @param ssimObject \code{\link{Session}}, \code{\link{Project}}, 
#' or \code{\link{SsimLibrary}} object
#' @param value character string of the new owner
#' 
#' @return 
#' A character string: the owner of the SsimObject. 
#' 
#' @examples 
#' \dontrun{
#' # Specify file path and name of new SsimLibrary
#' myLibraryName <- file.path(tempdir(), "testlib")
#' 
#' # Set up a SyncroSim Session, SsimLibrary, Project, and Scenario
#' mySession <- session()
#' myLibrary <- ssimLibrary(name = myLibraryName, session = mySession)
#' myProject <- project(myLibrary, project = "Definitions")
#' myScenario <- scenario(myProject, scenario = "My Scenario")
#' 
#' # Retrieve the owner of an SsimObject
#' owner(myLibrary)
#' owner(myProject)
#' owner(myScenario)
#' 
#' # Set the owner of a SyncroSim Scenario
#' owner(myScenario) <- "Apex RMS"
#' }
#' 
#' @export
setGeneric("owner", function(ssimObject) standardGeneric("owner"))

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

#' @rdname owner
setReplaceMethod(
  f = "owner",
  signature = "character",
  definition = function(ssimObject, value) {
    return(ssimObject)
  }
)

#' @rdname owner
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
