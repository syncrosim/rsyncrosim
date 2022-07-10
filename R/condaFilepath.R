# Copyright (c) 2021 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Path to Conda installation folder
#'
#' Gets or sets the path to the Conda installation folder. Can be used to direct
#' SyncroSim to a custom Conda installation.
#'
#' @param session \code{\link{Session}} object or character (i.e. filepath to a 
#' session). If \code{NULL}, \code{session()} will be used
#' @param value character. If empty, then returns the current Conda installation 
#' path
#' 
#' @return 
#' A character: the currently set filepath of the Conda installation folder.
#' 
#' @examples 
#' \donttest{
#' # Set up a SyncroSim Session
#' mySession <- session()
#' 
#' # Retrieve Conda installation path for the SyncroSim Session
#' condaFilepath(mySession)
#' 
#' # Set the Conda installation path for the SyncroSim Session
#' condaFilepath(mySession) <- "C:/miniconda3"
#' }
#' 
#' @export
setGeneric("condaFilepath", function(session) standardGeneric("condaFilepath"))

#' @rdname condaFilepath
setMethod("condaFilepath", signature(session = "Session"), function(session) session@condaFilepath)

#' @rdname condaFilepath
setMethod("condaFilepath", signature(session = "missingOrNULLOrChar"), function(session) {
  if (class(session) == "character") {
    session <- .session(session)
  } else {
    session <- .session()
  }
  if ((class(session) == "character") && (session == SyncroSimNotFound(warn = FALSE))) {
    return(SyncroSimNotFound())
  }
  return(condaFilepath(session))
})

#' @rdname condaFilepath
#' @export
setGeneric("condaFilepath<-", function(session, value) standardGeneric("condaFilepath<-"))
#' @rdname condaFilepath
setReplaceMethod(
  f = "condaFilepath",
  signature = "character",
  definition = function(session, value) {
    return(session)
  }
)

#' @rdname condaFilepath
setReplaceMethod(
  f = "condaFilepath",
  signature = "Session",
  definition = function(session, value) {
    session@condaFilepath <- value
    return(session)
  }
)
