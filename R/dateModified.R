# Copyright (c) 2024 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Last date a SsimLibrary, Project, Scenario, or Folder was modified
#'
#' The most recent modification date of a \code{\link{SsimLibrary}}, 
#' \code{\link{Project}}, \code{\link{Scenario}} or \code{\link{Folder}}.
#'
#' @param ssimObject  \code{\link{SsimLibrary}}, \code{\link{Project}},
#'     \code{\link{Scenario}}, or \code{\link{Folder}} object
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
  projInfo <- project(ssimObject, summary = TRUE)
  return(projInfo$LastModified)
})

#' @rdname dateModified
setMethod("dateModified", signature(ssimObject = "Scenario"), function(ssimObject) {
  scnInfo <- scenario(ssimObject, summary = TRUE)
  return(scnInfo$LastModified)
})

#' @rdname dateModified
setMethod("dateModified", signature(ssimObject = "Folder"), function(ssimObject) {
  folderInfo <- folder(ssimObject, summary = TRUE)
  return(folderInfo$LastModified)
})
