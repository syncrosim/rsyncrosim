# Copyright (c) 2023 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Read-only status of a SsimLibrary, Project, Scenario or Folder
#'
#' Retrieves or sets whether or not a \code{\link{SsimLibrary}}, 
#' \code{\link{Project}}, \code{\link{Scenario}}, or \code{\link{Folder}} is
#' read-only.
#'
#' @param ssimObject \code{\link{Scenario}}, \code{\link{Project}}, 
#' \code{\link{SsimLibrary}}, or \code{\link{Folder}} object
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
#' # Set up a SyncroSim Session, SsimLibrary, Project, Scenario, and Folder
#' mySession <- session()
#' myLibrary <- ssimLibrary(name = myLibraryName, session = mySession)
#' myProject <- project(myLibrary, project = "Definitions")
#' myScenario <- scenario(myProject, scenario = "My Scenario")
#' myFolder <- folder(myProject, "My Folder")
#' 
#' # Retrieve the read-only status of a SsimObject
#' readOnly(myLibrary)
#' readOnly(myProject)
#' readOnly(myScenario)
#' readOnly(myFolder)
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
  readOnlyStatus <- scnInfo$IsReadOnly
  if (readOnlyStatus == "No") {
    return(FALSE)
  } else {
    return(TRUE)
  }
})

#' @rdname readOnly
setMethod("readOnly", signature(ssimObject = "Scenario"), function(ssimObject) {
  scnInfo <- scenario(ssimObject, summary = TRUE)
  readOnlyStatus <- scnInfo$IsReadOnly
  if (readOnlyStatus == "No") {
    return(FALSE)
  } else {
    return(TRUE)
  }
})

#' @rdname readOnly
setMethod("readOnly", signature(ssimObject = "Folder"), function(ssimObject) {
  info <- getFolderData(ssimObject)
  readOnlyStatus <- info$ReadOnly #TODO: make this match others?
  if (readOnlyStatus == "No") {
    return(FALSE)
  } else {
    return(TRUE)
  }
})

#' @rdname readOnly
#' @export
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
    if (!is(value, "logical")) {
      stop("readOnly must be TRUE or FALSE.")
    }
    if (value == TRUE) {
      readOnly <- "yes"
    } else {
      readOnly <- "no"
    }
    args <- list(setprop = NULL, lib = .filepath(ssimObject), readonly = readOnly)
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

#' @rdname readOnly
setReplaceMethod(
  f = "readOnly",
  signature = "Folder",
  definition = function(ssimObject, value) {
    if (!is(value, "logical")) {
      stop("readOnly must be TRUE or FALSE.")
    }
    if (value == TRUE) {
      readOnly <- "yes"
    } else {
      readOnly <- "no"
    }
    args <- list(setprop = NULL, lib = .filepath(ssimObject), 
                 readonly = readOnly, fid = .folderId(ssimObject))
    tt <- command(args, .session(ssimObject))
    if (!identical(tt, "saved")) {
      stop(tt)
    }
    return(ssimObject)
  }
)
