# Copyright (c) 2019 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# GPL v.3 License
#' @include AAAClassDefinitions.R
NULL

#' Description of a SsimLibrary, Project or Scenario.
#'
#' Returns a description of a \code{\link{SsimLibrary}}, \code{\link{Project}} or 
#' \code{\link{Scenario}}.
#'
#' @param ssimObject An object of class SsimLibrary, Project or Scenario.
#' 
#' @return
#' A character string describing the ssimObject.
#' 
#' @examples 
#' \donttest{
#' temp_dir <- tempdir()
#' mySession <- session()
#' myLibrary <- ssimLibrary(name = file.path(temp_dir,"testlib"), session = mySession)
#' myProject <- project(myLibrary)
#' 
#' mydescription <- description(myProject)
#' }
#' 
#' @export
setGeneric("description", function(ssimObject) standardGeneric("description"))

#' Set the description of a SsimLibrary, Project or Scenario.
#'
#' Set the description of a \code{\link{SsimLibrary}}, \code{\link{Project}} or 
#' \code{\link{Scenario}}.
#'
#' @param ssimObject Scenario/Project/SsimLibrary.
#' @param value The new description.
#' 
#' @return
#' The object with updated description.
#' 
#' @examples 
#' \donttest{
#' temp_dir <- tempdir()
#' mySession <- session()
#' myLibrary <- ssimLibrary(name = file.path(temp_dir,"testlib"), session = mySession)
#' myProject <- project(myLibrary)
#' 
#' description(myProject) <- "my description"
#' }
#' 
#' @export
setGeneric("description<-", function(ssimObject, value) standardGeneric("description<-"))

#' @rdname description
setMethod("description", signature(ssimObject = "character"), function(ssimObject) {
  return(SyncroSimNotFound(ssimObject))
})

#' @rdname description
setMethod("description", signature(ssimObject = "SsimObject"), function(ssimObject) {
  # ssimObject=myLibrary
  if (class(ssimObject) == "SsimLibrary") {
    desc <- command(list(list = NULL, description = NULL, lib = .filepath(ssimObject)), session = .session(ssimObject))
  }
  if (class(ssimObject) == "Project") {
    desc <- command(list(list = NULL, description = NULL, lib = .filepath(ssimObject), pid = .projectId(ssimObject)), session = .session(ssimObject))
  }
  if (class(ssimObject) == "Scenario") {
    desc <- command(list(list = NULL, description = NULL, lib = .filepath(ssimObject), sid = .scenarioId(ssimObject)), session = .session(ssimObject))
  }

  while (max(grepl("  ", desc, fixed = TRUE))) {
    desc <- gsub("  ", " ", desc, fixed = TRUE)
  }
  desc <- gsub(". ", ".", desc, fixed = TRUE)

  desc <- desc[2:length(desc)]

  return(desc)
})

#' @rdname description-set
setReplaceMethod(
  f = "description",
  signature = "character",
  definition = function(ssimObject, value) {
    return(ssimObject)
  }
)

#' @rdname description-set
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
