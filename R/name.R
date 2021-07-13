# Copyright (c) 2019 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# GPL v.3 License
#' @include AAAClassDefinitions.R
NULL

#' Retrieves the name of a SsimLibrary, Project or Scenario.
#'
#' Retrieves the name of a \code{\link{SsimLibrary}}, \code{\link{Project}} or 
#' \code{\link{Scenario}}.
#'
#' @param ssimObject An object of class \code{\link{Session}}, \code{\link{Project}}, 
#' or \code{\link{SsimLibrary}}.
#' 
#' @return 
#' A character string: the name of the ssimObject.
#' 
#' @examples 
#' \donttest{
#' temp_dir <- tempdir()
#' mySession <- session()
#' myLibrary <- ssimLibrary(name = file.path(temp_dir,"testlib"), session = mySession)
#' myProject <- project(myLibrary, project = "Definitions")
#' myScenario <- scenario(myProject, scenario = "My Scenario")
#' 
#' name(myLibrary)
#' name(myProject)
#' name(myScenario)
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
  return(scnInfo$name)
})

#' @rdname name
setMethod("name", signature(ssimObject = "Project"), function(ssimObject) {
  info <- project(ssimObject, summary = TRUE)
  return(info$name)
})


#' Set ssimObject name.
#'
#' Set the name of a \code{\link{SsimLibrary}}, \code{\link{Project}}, or
#'  \code{\link{Scenario}}.
#'
#' @param ssimObject An object of class \code{\link{Session}}, \code{\link{Project}}, 
#' or \code{\link{SsimLibrary}}.
#' @param value A character string, the new name.
#' 
#' @return 
#' The updated ssim Object.
#' 
#' @examples 
#' \donttest{
#' temp_dir <- tempdir()
#' mySession <- session()
#' myLibrary <- ssimLibrary(name = file.path(temp_dir,"testlib"), session = mySession)
#' myProject <- project(myLibrary, project = "Definitions")
#' myScenario <- scenario(myProject, scenario = "My Scenario")
#' 
#' name(myScenario) <- "Scenario Test"
#' }
#' 
#' @export
setGeneric("name<-", function(ssimObject, value) standardGeneric("name<-"))

#' @rdname name-set
setReplaceMethod(
  f = "name",
  signature = "character",
  definition = function(ssimObject, value) {
    return(ssimObject)
  }
)

#' @rdname name-set
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

#' @rdname name-set
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

#' @rdname name-set
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
