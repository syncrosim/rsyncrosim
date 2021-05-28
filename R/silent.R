# Copyright (c) 2019 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# GPL v.3 License
#' @include AAAClassDefinitions.R
NULL

#' Check if a Session is silent
#'
#' Checks whether a SyncroSim \code{\link{Session}} is silent or not.
#'
#' @param session Session or character. An object of class \code{\link{Session}} 
#'     or path to a session. If NULL, the default session will be used.
#' 
#' @return 
#' Returns a logical value: `TRUE` if the session is silent and `FALSE` otherwise.
#' 
#' @examples 
#' \donttest{
#' temp_dir <- tempdir()
#' mySession <- session()
#' 
#' silent(mySession)
#' }
#' 
#' @export
setGeneric("silent", function(session) standardGeneric("silent"))

#' @rdname silent
setMethod("silent", signature(session = "Session"), function(session) session@silent)

#' @rdname silent
setMethod("silent", signature(session = "missingOrNULLOrChar"), function(session) {
  if (class(session) == "character") {
    session <- .session(session)
  } else {
    session <- .session()
  }
  if ((class(session) == "character") && (session == SyncroSimNotFound(warn = FALSE))) {
    return(SyncroSimNotFound())
  }
  return(silent(session))
})

#' Set silent property of a Session
#'
#' Set silent property of a \code{\link{Session}} to TRUE or FALSE.
#'
#' @param session An object of class \code{\link{Session}}
#' @param value Logical.
#' 
#' @return 
#' The updated ssimObject.
#' 
#' @examples 
#' \donttest{
#' temp_dir <- tempdir()
#' mySession <- session()
#' 
#' silent(mySession) <- TRUE
#' }
#' 
#' @export
setGeneric("silent<-", function(session, value) standardGeneric("silent<-"))
#' @rdname silent-set
setReplaceMethod(
  f = "silent",
  signature = "character",
  definition = function(session, value) {
    return(session)
  }
)

#' @rdname silent-set
setReplaceMethod(
  f = "silent",
  signature = "Session",
  definition = function(session, value) {
    session@silent <- value
    return(session)
  }
)
