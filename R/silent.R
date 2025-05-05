# Copyright (c) 2024 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Silent status of SyncroSim Session
#'
#' Checks or sets whether a SyncroSim \code{\link{Session}} is silent or not. In
#' a silent session, warnings from the console are ignored.
#'
#' @param session \code{\link{Session}} object or character (i.e. filepath to a 
#' session). If \code{NULL}, \code{session()} will be used
#' @param value logical. If \code{TRUE} (default), the SyncroSim Session will be 
#' silent
#' 
#' @return 
#' A logical: \code{TRUE} if the session is silent and \code{FALSE} otherwise.
#' 
#' @examples 
#' \dontrun{
#' # Set up a SyncroSim Session
#' mySession <- session()
#' 
#' # Check the silent status of a SyncroSim Session
#' silent(mySession)
#' 
#' # Set the silent status of a SyncroSim Session
#' silent(mySession) <- FALSE
#' }
#' 
#' @export
setGeneric("silent", function(session) standardGeneric("silent"))

#' @rdname silent
setMethod("silent", signature(session = "Session"), function(session) session@silent)

#' @rdname silent
setMethod("silent", signature(session = "missingOrNULLOrChar"), function(session) {
  if (is(session, "character")) {
    session <- .session(session)
  } else {
    session <- .session()
  }
  if ((is(session, "character")) && (is(session, SyncroSimNotFound(warn = FALSE)))) {
    return(SyncroSimNotFound())
  }
  return(silent(session))
})

#' @rdname silent
#' @export
setGeneric("silent<-", function(session, value) standardGeneric("silent<-"))
#' @rdname silent
setReplaceMethod(
  f = "silent",
  signature = "character",
  definition = function(session, value) {
    return(session)
  }
)

#' @rdname silent
setReplaceMethod(
  f = "silent",
  signature = "Session",
  definition = function(session, value) {
    session@silent <- value
    return(session)
  }
)
