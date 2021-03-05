# Copyright (c) 2019 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# GPL v.3 License
#' @include AAAClassDefinitions.R
NULL

#' The name of a SsimLibrary, Project or Scenario.
#'
#' Retrieves the name of a \code{\link{SsimLibrary}}, \code{\link{Project}} or \code{\link{Scenario}}.
#'
#' @param ssimObject SsimLibrary, Project, or Scenario.
#' 
#' @return 
#' Character string: the name of the ssimObject.
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
#' Set the name of a \code{\link{SsimLibrary}}, \code{\link{Project}}, or \code{\link{Scenario}}.
#'
#' @param ssimObject SsimLibrary, Project, or Scenario.
#' 
#' @param value 
#' The updated ssimObject. 
#' 
#' @return 
#' The updated ssim Object.
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
