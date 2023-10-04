# Copyright (c) 2023 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Retrieves SyncroSim version
#'
#' Retrieves the version of a SyncroSim Session.
#'
#' @param session \code{\link{Session}} object
#' 
#' @return
#' A character string e.g. "2.2.13".
#' 
#' @examples 
#' \dontrun{
#' # Set SyncroSim Session
#' mySession <- session()
#' 
#' # Retrieve version of SyncroSim associated with Session
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
