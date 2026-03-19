# Copyright (c) 2024 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Backup a SsimLibrary
#'
#' Backup a \code{\linkS4class{SsimLibrary}}. The backup folder can be defined in the
#' SyncroSim User Interface, but is by default at the same level as the 
#' SsimLibrary file, and is called libraryName.backup.
#'
#' @param ssimObject \code{\linkS4class{SsimLibrary}}, 
#'     \code{\linkS4class{Project}} or \code{\linkS4class{Scenario}} object
#' 
#' @return 
#' Invisibly returns \code{TRUE} upon success (i.e.successful 
#' backup) and \code{FALSE} upon failure.
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
#' # Back up data from the SsimLibrary
#' backup(myLibrary)
#' }
#' 
#' @export
setGeneric("backup", function(ssimObject) standardGeneric("backup"))

#' @rdname backup
setMethod("backup", signature(ssimObject = "character"), function(ssimObject) {
  return(SyncroSimNotFound(ssimObject))
})

#' @rdname backup
setMethod("backup", signature(ssimObject = "SsimObject"), function(ssimObject) {
  success <- FALSE
  ds <- datasheet(ssimObject, name = "core_Backup")
  args <- list(lib = .filepath(ssimObject), backup = NULL)

  if (!is.na(ds$IncludeData)) {
    if (ds$IncludeData) {
      args <- c(args, list(extdata = NULL))
    }
  }

  tt <- command(args = args, session = .session(ssimObject))
  message(tt)
  if (tt == "Backup complete."){
    success <- TRUE
  } else {
    success <- FALSE
  }
  return(invisible(success))
})
