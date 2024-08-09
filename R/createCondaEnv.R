# Copyright (c) 2024 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Create SyncroSim package conda environments
#'
#' Creates the conda environment for the specified SyncroSim package(s).
#'
#' @param pkgs character or list of characters. 
#' @param session \code{\link{Session}} object or character (i.e. filepath to a 
#' session). If \code{NULL}, \code{session()} will be used
#'  
#' @return 
#' Invisibly returns \code{TRUE} upon success (i.e.successful creation of the 
#' conda environment(s)) or \code{FALSE} upon failure.
#' 
#' @examples 
#' \dontrun{
#' # Set up a SyncroSim Session
#' mySession <- session()
#' 
#' # Create the conda environment for helloworldConda package
#' condaFilepath(pkgs = "helloworldConda", mySession)
#' }
#' 
#' @export
setGeneric("createCondaEnv", function(pkgs, session = NULL) standardGeneric("createCondaEnv"))

#' @rdname createCondaEnv
setMethod("createCondaEnv", signature(session = "character"), function(pkgs, session) {
  return(SyncroSimNotFound(session))
})

#' @rdname createCondaEnv
setMethod("createCondaEnv", signature(session = "missingOrNULL"), function(pkgs, session) {
  session <- .session()
  return(createCondaEnv(session))
})

#' @rdname createCondaEnv
setMethod("createCondaEnv", signature(session = "Session"), function(pkgs, session) {
  
  message("Creating Conda environments. Please wait...")
  
  # Check if environment needs to be created, create if doesn't exist yet
  for (package in pkgs) {
    tt <- command(list(conda = NULL, createenv = NULL, pkg = package), session)
    if (length(tt) > 1){
      if (!grepl("Creating Conda environments", tt[1], fixed = TRUE)){
        stop(tt[1])
      }
    } else {
      if (grepl("No Conda installation found", tt, fixed = TRUE)) {
        errorMessage = "Conda must be installed to use Conda environments. See ?installConda for details."
      } else {
        errorMessage = tt
      }
      
      message(errorMessage)
      return(invisible(TRUE))
    }
  }
  
  return(invisible(TRUE))
})
