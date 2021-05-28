# Copyright (c) 2019 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# GPL v.3 License
#' @include AAAClassDefinitions.R
NULL

#' Merge Dependencies for a Scenario.
#'
#' Retrieves whether or not a \code{\link{Scenario}} is configured to merge 
#' dependencies at run time.
#'
#' @param ssimObject An object of class \code{\link{Scenario}}.
#' 
#' @return 
#' Returns a logical: `TRUE` is the scenario is configured to merge dependencies at run time, 
#' and `FALSE` otherwise.
#' 
#' @examples 
#' \donttest{
#' temp_dir <- tempdir()
#' mySession <- session()
#' myLibrary <- ssimLibrary(name = file.path(temp_dir,"testlib"), session = mySession)
#' myProject <- project(myLibrary)
#' myScenario <- scenario(myProject)
#' 
#' mergeDependencies(myScenario)
#' }
#' 
#' @export
setGeneric("mergeDependencies", function(ssimObject) standardGeneric("mergeDependencies"))

#' @rdname mergeDependencies
setMethod("mergeDependencies", signature(ssimObject = "character"), function(ssimObject) {
  return(SyncroSimNotFound(ssimObject))
})

#' @rdname mergeDependencies
setMethod("mergeDependencies", signature(ssimObject = "Scenario"), function(ssimObject) {
  scnInfo <- scenario(ssimObject, summary = TRUE)
  if (scnInfo$mergeDependencies == "Yes"){
    value <- TRUE
  } else if (scnInfo$mergeDependencies == "No"){
    value <- FALSE
  }
  return(value)
})

#' Merge Dependencies for a Scenario.
#'
#' Sets whether or not a \code{\link{Scenario}} is configured to merge 
#' dependencies at run time.
#'
#' @param ssimObject An object of class \code{\link{Scenario}}.
#' @param value Logical. If TRUE the Scenario will be set to merge dependencies at run time.
#' 
#' @return 
#' Returns the updated ssimObject.
#' 
#' @examples 
#' \donttest{
#' temp_dir <- tempdir()
#' mySession <- session()
#' myLibrary <- ssimLibrary(name = file.path(temp_dir,"testlib"), session = mySession)
#' myProject <- project(myLibrary)
#' myScenario <- scenario(myProject)
#' 
#' mergeDependencies(myScenario) <- TRUE
#' }
#' 
#' @export
setGeneric("mergeDependencies<-", function(ssimObject, value) standardGeneric("mergeDependencies<-"))

#' @rdname mergeDependencies-set
setReplaceMethod(
  f = "mergeDependencies",
  signature = "character",
  definition = function(ssimObject, value) {
    return(ssimObject)
  }
)

#' @rdname mergeDependencies-set
setReplaceMethod(
  f = "mergeDependencies",
  signature = "Scenario",
  definition = function(ssimObject, value) {
    if (class(value) != "logical") {
      stop("mergeDependencies must be TRUE or FALSE.")
    }
    if (value == TRUE) {
      mergeDeps <- "yes"
    } else {
      mergeDeps <- "no"
    }
    args <- list(setprop = NULL, lib = .filepath(ssimObject), mergedeps = mergeDeps, sid = .scenarioId(ssimObject))
    tt <- command(args, .session(ssimObject))
    if (!identical(tt, "saved")) {
      stop(tt)
    }
    return(ssimObject)
  }
)
