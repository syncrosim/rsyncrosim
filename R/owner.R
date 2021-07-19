# Copyright (c) 2021 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# GPL v.3 License
#' @include AAAClassDefinitions.R
NULL

#' Retrieves the owner of a SsimLibrary, Project or Scenario.
#'
#' Retrieves the owner of a \code{\link{SsimLibrary}}, \code{\link{Project}} or 
#' \code{\link{Scenario}}.
#'
#' @param ssimObject An object of class \code{\link{Session}}, \code{\link{Project}}, 
#' or \code{\link{SsimLibrary}}.
#' 
#' @return 
#' A character string: the owner of the ssimObject. 
#' 
#' @examples 
#' \donttest{
#' temp_dir <- tempdir()
#' mySession <- session()
#' myLibrary <- ssimLibrary(name = file.path(temp_dir,"testlib"), session = mySession)
#' myProject <- project(myLibrary, project = "Definitions")
#' myScenario <- scenario(myProject, scenario = "My Scenario")
#' 
#' owner(myLibrary)
#' owner(myProject)
#' owner(myScenario)
#' }
#' 
#' @export
setGeneric("owner", function(ssimObject) standardGeneric("owner"))

#' Set the owner of a SsimLibrary, Project or Scenario.
#'
#' Set the owner of a \code{\link{SsimLibrary}}, \code{\link{Project}} or 
#' \code{\link{Scenario}}.
#'
#' @param ssimObject An object of class \code{\link{Session}}, \code{\link{Project}}, 
#' or \code{\link{SsimLibrary}}.
#' @param value A character string, the new owner.
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
#' owner(myLibrary) <- "Apex RMS"
#' }
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
