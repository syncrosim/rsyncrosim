# Copyright (c) 2024 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Apply updates
#'
#' Apply updates to a \code{\link{SsimLibrary}}, or a \code{\link{Project}} or 
#' \code{\link{Scenario}} associated with a SsimLibrary.
#'
#' @param ssimObject \code{\link{Session}}, \code{\link{Project}}, 
#' or \code{\link{SsimLibrary}} object. If \code{NULL} (default), 
#' \code{session()} will be used
#' 
#' @return 
#' Invisibly returns \code{TRUE} upon success (i.e.successful 
#' update) and \code{FALSE} upon failure.
#' 
#' @examples 
#' \donttest{
#' # Set the file path and name of the new SsimLibrary
#' myLibraryName <- file.path(tempdir(),"testlib")
#' 
#' # Set the SyncroSim Session, SsimLibrary, and Project
#' mySession <- session()
#' myLibrary <- ssimLibrary(name = myLibraryName, session = mySession,
#'                          overwrite=TRUE)
#' myProject <- project(myLibrary, project = "My Project")
#' 
#' # Update Project
#' ssimUpdate(myProject)
#' 
#' # Create Scenario
#' myScenario <- scenario(myLibrary, scenario = "My Scenario")
#' 
#' # Update scenario
#' ssimUpdate(myScenario)
#' }
#' 
#' @export
setGeneric("ssimUpdate", function(ssimObject) standardGeneric("ssimUpdate"))

#' @rdname ssimUpdate
setMethod("ssimUpdate", signature(ssimObject = "character"), function(ssimObject) {
  return(SyncroSimNotFound(ssimObject))
})

#' @rdname ssimUpdate
setMethod("ssimUpdate", signature(ssimObject = "SsimObject"), function(ssimObject) {
  success <- FALSE
  tt <- command(list(update = NULL, lib = .filepath(ssimObject)), .session(ssimObject))
  if (!is.na(tt[1])){ 
    if (tt == "saved"){
      message("Library successfully updated")
      success <- TRUE
    } else{
      message(tt)
    }
  }
  return(invisible(success))
})
  