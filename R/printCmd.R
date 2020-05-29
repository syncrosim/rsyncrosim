# Copyright (c) 2019 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# GPL v.3 License
#' @include AAAClassDefinitions.R
NULL

#' Get printCmd of a Session.
#'
#' Retrives a printCmd setting of a Session object.
#'
#' @param session Session or character. A Session object or path to a session. 
#' If NULL, the default session will be used.
#' 
#' @return 
#' Returns a logical value: `TRUE` is the session is configured to print commands and 
#' `FALSE` if it is not.
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
