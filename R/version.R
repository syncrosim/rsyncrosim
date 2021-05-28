# Copyright (c) 2019 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# GPL v.3 License
#' @include AAAClassDefinitions.R
NULL

#' Retrieves the SyncroSim version
#'
#' Retrieves the version of a SyncroSim Session.
#'
#' @param session An object of class \code{\link{Session}}.
#' 
#' @return
#' A character string e.g. "2.2.13".
#' 
#' @examples 
#' \donttest{
#' temp_dir <- tempdir()
#' mySession <- session()
#' 
#' version(mySession)
#' }
#' 
#' @export
setGeneric("version", function(session = NULL) standardGeneric("version"))

#' @rdname version
setMethod("version", signature(session = "character"), function(session) {
  return(SyncroSimNotFound(session))
})

#' @rdname version
setMethod("version", signature(session = "missingOrNULL"), function(session) {
  return(version(session()))
})

#' @rdname version
setMethod("version", signature(session = "Session"), function(session) {
  version <- command(list(version = NULL), session)
  version <- gsub("Version is: ", "", version, fixed = TRUE)
  return(version)
})
