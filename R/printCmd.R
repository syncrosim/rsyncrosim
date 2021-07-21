# Copyright (c) 2021 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# GPL v.3 License
#' @include AAAClassDefinitions.R
NULL

#' Retrieves a printCmd setting of a Session.
#'
#' Retrieves a printCmd setting of a \code{\link{Session}} object. The printCmd
#' setting configures a session for printing commands sent to the console.
#'
#' @param session Session or character. An object of class \code{\link{Session}} 
#' or path to a session. If NULL, the default session will be used.
#' 
#' @return 
#' Returns a logical value: `TRUE` if the session is configured to print commands and 
#' `FALSE` if it is not.
#' 
#' @examples 
#' \donttest{
#' temp_dir <- tempdir()
#' mySession <- session()
#' 
#' printCmd(mySession)
#' }
#' 
#' @export
setGeneric("printCmd", function(session = NULL) standardGeneric("printCmd"))

#' @rdname printCmd
setMethod("printCmd", signature(session = "Session"), function(session) session@printCmd)

#' @rdname printCmd
setMethod("printCmd", signature(session = "missingOrNULLOrChar"), function(session) {
  if (class(session) == "character") {
    session <- .session(session)
  } else {
    session <- .session()
  }
  if ((class(session) == "character") && (session == SyncroSimNotFound(warn = FALSE))) {
    return(SyncroSimNotFound())
  }
  return(printCmd(session))
})
