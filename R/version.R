# Copyright (c) 2019 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# GPL v.3 License
#' @include AAAClassDefinitions.R
NULL

#' The SyncroSim version
#'
#' The version of a SyncroSim Session.
#'
#' @param session \code{\link{Session}}.
#' 
#' @return
#' A character string e.g. "2.2.13".
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
