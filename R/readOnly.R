# Copyright (c) 2021 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# GPL v.3 License
#' @include AAAClassDefinitions.R
NULL

#' Retrieves the Read-only status of a SsimLibrary, Project or Scenario.
#'
#' Whether or not a \code{\link{SsimLibrary}}, \code{\link{Project}} or 
#' \code{\link{Scenario}} is read-only is read-only.
#'
#' @param ssimObject An object of class \code{\link{Session}}, \code{\link{Project}}, 
#' or \code{\link{SsimLibrary}}.
#' 
#' @return 
#' Returns a logical value: `TRUE` if the ssimObject is read only and `FALSE`
#' otherwise.
#' 
#' @examples 
#' \donttest{
#' temp_dir <- tempdir()
#' mySession <- session()
#' myLibrary <- ssimLibrary(name = file.path(temp_dir,"testlib"), session = mySession)
#' myProject <- project(myLibrary, project = "Definitions")
#' myScenario <- scenario(myProject, scenario = "My Scenario")
#' 
#' readOnly(myLibrary)
#' readOnly(myProject)
#' readOnly(myScenario)
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

#' Set the read/write status of a SsimLibrary, Project or Scenario.
#'
#' Set the read-only status of a \code{\link{SsimLibrary}}, \code{\link{Project}} or \code{\link{Scenario}}.
#' Applies to child objects if ssimObject is a SsimLibrary or Project.
#'
#' @param ssimObject An object of class \code{\link{Session}}, \code{\link{Project}}, 
#' or \code{\link{SsimLibrary}}.
#' @param value Logical. If `TRUE` the ssimObject will be read-only.
#' 
#' @return 
#' The updated ssimObject.
#' 
#' @examples 
#' \donttest{
#' temp_dir <- tempdir()
#' mySession <- session()
#' myLibrary <- ssimLibrary(name = file.path(temp_dir,"testlib"), session = mySession)
#' myProject <- project(myLibrary, project = "Definitions")
#' myScenario <- scenario(myProject, scenario = "My Scenario")
#' 
#' readOnly(myLibrary) <- FALSE
#' readOnly(myProject) <- FALSE
#' readOnly(myScenario) <- TRUE
#' }
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
