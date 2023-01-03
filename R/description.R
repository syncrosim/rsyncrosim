# Copyright (c) 2023 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Description of SsimLibrary, Project or Scenario
#'
#' Get or set the description of a \code{\link{SsimLibrary}}, \code{\link{Project}} or 
#' \code{\link{Scenario}}.
#'
#' @param ssimObject \code{\link{SsimLibrary}}, \code{\link{Project}} or 
#' \code{\link{Scenario}} object
#' @param value character string specifying the new description
#' 
#' @return
#' A character string: the description of the SsimObject
#' 
#' @examples 
#' \donttest{
#' # Specify file path and name of new SsimLibrary
#' myLibraryName <- file.path(tempdir(), "testlib")
#' 
#' # Set up a SyncroSim Session, SsimLibrary, and Project
#' mySession <- session()
#' myLibrary <- ssimLibrary(name = myLibraryName, session = mySession)
#' myProject <- project(myLibrary, project = "Definitions")
#' 
#' # Retrieve the description of the SyncroSim Project
#' mydescription <- description(myProject)
#' 
#' # Set the description of the SyncroSim Project
#' description(myProject) <- "my description"
#' }
#' 
#' @export
setGeneric("description", function(ssimObject) standardGeneric("description"))

#' @rdname description
#' @export
setGeneric("description<-", function(ssimObject, value) standardGeneric("description<-"))

#' @rdname description
setMethod("description", signature(ssimObject = "character"), function(ssimObject) {
  return(SyncroSimNotFound(ssimObject))
})

#' @rdname description
setMethod("description", signature(ssimObject = "SsimObject"), function(ssimObject) {
  # ssimObject=myLibrary
  if (is(ssimObject, "SsimLibrary")) {
    desc <- command(list(list = NULL, description = NULL, lib = .filepath(ssimObject)), session = .session(ssimObject))
  }
  if (is(ssimObject, "Project")) {
    desc <- command(list(list = NULL, description = NULL, lib = .filepath(ssimObject), pid = .projectId(ssimObject)), session = .session(ssimObject))
  }
  if (is(ssimObject, "Scenario")) {
    desc <- command(list(list = NULL, description = NULL, lib = .filepath(ssimObject), sid = .scenarioId(ssimObject)), session = .session(ssimObject))
  }

  while (max(grepl("  ", desc, fixed = TRUE))) {
    desc <- gsub("  ", " ", desc, fixed = TRUE)
  }
  desc <- gsub(". ", ".", desc, fixed = TRUE)

  desc <- desc[2:length(desc)]

  return(desc)
})

#' @rdname description
setReplaceMethod(
  f = "description",
  signature = "character",
  definition = function(ssimObject, value) {
    return(ssimObject)
  }
)

#' @rdname description
setReplaceMethod(
  f = "description",
  signature = "SsimObject",
  definition = function(ssimObject, value) {
    inValue <- value
    if (length(inValue) > 1) {
      value <- ""
      for (i in 1:length(inValue)) {
        value <- paste0(value, inValue[[i]], sep = "\n")
      }
    }
    value <- gsub("\n", "\\n", value, fixed = TRUE)
    args <- list(setprop = NULL, lib = .filepath(ssimObject), description = value)
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
