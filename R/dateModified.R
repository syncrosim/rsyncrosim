# Copyright (c) 2021 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Last date a SsimLibrary, Project or Scenario was modified
#'
#' The most recent modification date of a \code{\link{SsimLibrary}}, 
#' \code{\link{Project}} or \code{\link{Scenario}}.
#'
#' @param ssimObject  \code{\link{SsimLibrary}}, \code{\link{Project}},
#'     or \code{\link{Scenario}} object
#' 
#' @return 
#' A character string: date and time of the most recent modification 
#' to the SsimObject provided as input.
#' 
#' @examples
#' \dontrun{
#' # Specify file path and name of new SsimLibrary
#' myLibraryName <- file.path(tempdir(), "testlib")
#' 
#' # Set up a SyncroSim Session and SsimLibrary
#' mySession <- session()
#' myLibrary <- ssimLibrary(name = myLibraryName, session = mySession)
#' 
#' # Check the last date of modification of the SsimLibrary
#' dateModified(myLibrary)
#' }
#' 
#' @export
setGeneric("dateModified", function(ssimObject) standardGeneric("dateModified"))

#' @rdname dateModified
setMethod("dateModified", signature(ssimObject = "character"), function(ssimObject) {
  return(SyncroSimNotFound(ssimObject))
})

#' @rdname dateModified
setMethod("dateModified", signature(ssimObject = "SsimLibrary"), function(ssimObject) {
  cInfo <- info(ssimObject)
  property <- NULL
  return(subset(cInfo, property == "Last Modified:")$value)
})

#' @rdname dateModified
setMethod("dateModified", signature(ssimObject = "Project"), function(ssimObject) {
  scnInfo <- project(ssimObject, summary = TRUE)
  return(scnInfo$lastModified)
})

#' @rdname dateModified
setMethod("dateModified", signature(ssimObject = "Scenario"), function(ssimObject) {
  scnInfo <- scenario(ssimObject, summary = TRUE)
  return(scnInfo$lastModified)
})
