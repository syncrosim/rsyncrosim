# Copyright (c) 2019 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# GPL v.3 License
#' @include AAAClassDefinitions.R
NULL

#' The last date a SsimLibrary, Project or Scenario was modified.
#'
#' The most recent modification date of a \code{\link{SsimLibrary}}, 
#' \code{\link{Project}} or \code{\link{Scenario}}.
#'
#' @param ssimObject  An object of class SsimLibrary, Project or Scenario.
#' 
#' @return 
#' A character string of the date and time of the most recent modification 
#' to the ssimObject provided as input.
#' 
#' @examples
#' \dontrun{
#' temp_dir <- tempdir()
#' myses <- session()
#' myLibrary <- ssimLibrary(name = file.path(temp_dir,"testlib"), session = myses)
#' 
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
