# Copyright (c) 2021 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Retrieves printCmd setting of a Session
#'
#' Retrieves a printCmd setting of a \code{\link{Session}} object. The printCmd
#' setting configures a Session for printing commands sent to the console.
#'
#' @param session Session object or character. The Session or path
#'  to a Session where the printCmd settings are retrieved from. If \code{NULL} (default),
#'  \code{session()} will be used
#' 
#' @return 
#' A logical : \code{TRUE} if the session is configured to print commands and 
#' \code{FALSE} if it is not.
#' 
#' @examples 
#' \donttest{
#' # Set SyncroSim Session
#' mySession <- session()
#' 
#' # Retrieve printCmd settings for given Session
#' printCmd(mySession)
#' }
#' 
#' @export
setGeneric("printCmd", function(session = NULL) standardGeneric("printCmd"))

#' @rdname printCmd
setMethod("printCmd", signature(session = "Session"), function(session) session@printCmd)

#' @rdname printCmd
setMethod("printCmd", signature(session = "missingOrNULLOrChar"), function(session) {
  if (is(session, "character")) {
    session <- .session(session)
  } else {
    session <- .session()
  }
  if ((is(session, "character")) && (is(session, SyncroSimNotFound(warn = FALSE)))) {
    return(SyncroSimNotFound())
  }
  return(printCmd(session))
})
