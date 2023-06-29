# Copyright (c) 2023 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Owner of a SsimLibrary, Project, Scenario, or Folder
#'
#' Retrieves or sets the owner of a \code{\link{SsimLibrary}},
#' \code{\link{Project}}, \code{\link{Scenario}}, or \code{\link{Folder}}.
#'
#' @param ssimObject \code{\link{Session}}, \code{\link{Project}}, 
#' \code{\link{SsimLibrary}}, or \code{\link{Folder}} object
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

#' @rdname owner
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
  projInfo <- project(ssimObject, summary = TRUE)
  return(projInfo$owner)
})

#' @rdname owner
setMethod("owner", signature(ssimObject = "Scenario"), function(ssimObject) {
  scnInfo <- scenario(ssimObject, summary = TRUE)
  return(scnInfo$owner)
})

#' @rdname owner
setMethod("owner", signature(ssimObject = "Folder"), function(ssimObject) {
  info <- getFolderData(ssimObject)
  return(info$Owner)
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
    if (is(ssimObject, "Project")) {
      args$pid <- .projectId(ssimObject)
    }
    if (is(ssimObject, "Scenario")) {
      args$sid <- .scenarioId(ssimObject)
    }
    tt <- command(args, .session(ssimObject))
    if (!identical(tt, "saved")) {
      stop(tt)
    }
    return(ssimObject)
  }
)

#' @rdname owner
setReplaceMethod(
  f = "owner",
  signature = "Folder",
  definition = function(ssimObject, value) {
    args <- list(setprop = NULL, lib = .filepath(ssimObject), owner = value, 
                 fid = .folderId(ssimObject))
    tt <- command(args, .session(ssimObject))
    if (!identical(tt, "saved")) {
      stop(tt)
    }
    return(ssimObject)
  }
)
