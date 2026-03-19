# Copyright (c) 2024 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Compacts a SsimLibrary
#'
#' Compact a \code{\linkS4class{SsimLibrary}}. Removes extraneous data from the SyncroSim library.
#'
#' @param ssimLibrary \code{\linkS4class{SsimLibrary}} object
#'
#' @return
#' Invisibly returns \code{TRUE} upon success (i.e.successful
#' compacting) and \code{FALSE} upon failure.
#'
#' @examples
#' \dontrun{
#' # Specify file path and name of new SsimLibrary
#' myLibraryName <- file.path(tempdir(), "testlib")
#'
#' # Set up a SyncroSim Session, SsimLibrary, and Project
#' mySession <- session()
#' myLibrary <- ssimLibrary(name = myLibraryName, session = mySession)
#'
#' # Compact data from the SsimLibrary
#' compact(myLibrary)
#' }
#'
#' @export
setGeneric("compact", function(ssimLibrary) standardGeneric("compact"))

#' @rdname compact
setMethod("compact", signature(ssimLibrary = "character"), function(ssimLibrary) {
  return(SyncroSimNotFound(ssimLibrary))
})

#' @rdname compact
setMethod("compact", signature(ssimLibrary = "SsimLibrary"), function(ssimLibrary) {

  success <- FALSE

  args <- list(compact = NULL, lib = filepath(ssimLibrary))

  tt <- command(args = args, session = .session(ssimLibrary))
  message(tt)
  if ("Library successfully compacted." %in% tt) {
    success <- TRUE
  } else {
    success <- FALSE
  }
  return(invisible(success))
})
