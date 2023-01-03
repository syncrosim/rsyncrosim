# Copyright (c) 2023 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Name of a SsimLibrary, Project or Scenario
#'
#' Retrieves or sets the name of a \code{\link{SsimLibrary}}, 
#' \code{\link{Project}} or \code{\link{Scenario}}.
#'
#' @param ssimObject \code{\link{Scenario}}, \code{\link{Project}}, 
#' or \code{\link{SsimLibrary}} object
#' @param value character string of the new name
#' 
#' @return 
#' A character string: the name of the SsimObject.
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
#' # Retrieve names of the SsimObjects
#' name(myLibrary)
#' name(myProject)
#' name(myScenario)
#' 
#' # Set the name of the SyncroSim Scenario
#' name(myScenario) <- "My Scenario Name"
#' }
#' 
#' @export
setGeneric("name", function(ssimObject) standardGeneric("name"))

#' @rdname name
setMethod("name", signature(ssimObject = "character"), function(ssimObject) {
  return(SyncroSimNotFound(ssimObject))
})

#' @rdname name
setMethod("name", signature(ssimObject = "SsimLibrary"), function(ssimObject) {
  cInfo <- info(ssimObject)
  property <- NULL
  return(subset(cInfo, property == "Name:")$value)
})

#' @rdname name
setMethod("name", signature(ssimObject = "Scenario"), function(ssimObject) {
  scnInfo <- scenario(ssimObject, summary = TRUE)
  return(scnInfo$Name)
})

#' @rdname name
setMethod("name", signature(ssimObject = "Project"), function(ssimObject) {
  info <- project(ssimObject, summary = TRUE)
  return(info$Name)
})

#' @rdname name
#' @export
setGeneric("name<-", function(ssimObject, value) standardGeneric("name<-"))

#' @rdname name
setReplaceMethod(
  f = "name",
  signature = "character",
  definition = function(ssimObject, value) {
    return(ssimObject)
  }
)

#' @rdname name
setReplaceMethod(
  f = "name",
  signature = "SsimLibrary",
  definition = function(ssimObject, value) {
    tt <- command(list(setprop = NULL, lib = .filepath(ssimObject), name = value), .session(ssimObject))
    if (!identical(tt, "saved")) {
      stop(tt)
    }
    return(ssimObject)
  }
)

#' @rdname name
setReplaceMethod(
  f = "name",
  signature = "Project",
  definition = function(ssimObject, value) {
    tt <- command(list(setprop = NULL, lib = .filepath(ssimObject), pid = .projectId(ssimObject), name = value), .session(ssimObject))
    if (!identical(tt, "saved")) {
      stop(tt)
    }
    return(ssimObject)
  }
)

#' @rdname name
setReplaceMethod(
  f = "name",
  signature = "Scenario",
  definition = function(ssimObject, value) {
    tt <- command(list(setprop = NULL, lib = .filepath(ssimObject), sid = .scenarioId(ssimObject), name = value), .session(ssimObject))
    if (!identical(tt, "saved")) {
      stop(tt)
    }
    return(ssimObject)
  }
)
