# Copyright (c) 2021 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Installs Miniconda
#'
#' This function installs Miniconda to either the default installation path
#' within the SyncroSim installation folder or a custom path.
#'
#' @param folderPath character string.  The file path to the Conda installation folder.
#' If \code{NULL}, then the default installation folder is used 
#' @param session \code{\link{Session}} object. If \code{NULL} (default),
#' \code{session()} will be used
#' 
#' @return 
#' Invisibly returns \code{TRUE} upon success (i.e.successful 
#' install) and \code{FALSE} upon failure.
#' 
#' @examples
#' \dontrun{
#' # Create a new SyncroSim Session
#' mySession <- session()
#' 
#' # Install Conda for the given SyncroSim session
#' installConda(folderPath = "C:/miniconda3", mySession)
#' }
#' 
#' @export
setGeneric("installConda", function(folderPath, session = NULL) standardGeneric("installConda"))

#' @rdname installConda
setMethod("installConda", signature(session = "character"), function(folderPath, session) {
  return(SyncroSimNotFound(session))
})

#' @rdname installConda
setMethod("installConda", signature(session = "missingOrNULL"), function(folderPath, session) {
  session <- .session()
  return(installConda(folderPath, session))
})

#' @rdname installConda
setMethod("installConda", signature(session = "Session"), function(folderPath, session) {
  success <- FALSE
  
  if (!is.null(folderPath)) {
    
    condaFilepath(session) <- folderPath
    tt <- command(args = list(conda = NULL, path = folderPath))
    args <- list(conda = NULL, install = NULL, path = folderPath)
    
  } else {
    
    presetCondaPath <- condaFilepath(session)
    if (presentCondaPath != "default") {
      tt <- command(args = list(conda = NULL, path = presetCondaPath))
      args <- list(conda = NULL, install = NULL, path = presetCondaPath)
    } else {
      args <- list(conda = NULL, install = NULL)
    }
    
  }
  
  installRunning <- TRUE
  time <- 0
  
  while (installRunning & time < 600) {
    tt <- command(args, session)
    if (endsWith(tt[2], "Running Conda Installer.  Please wait...")){
      message("Running Conda Installer.  Please wait...")
    } else {
      installRunning <- FALSE
    }
    
    if (length(tt) > 2) {
      installRunning <- FALSE
      message(tt[3])
    }
    
    addTime <- 5
    Sys.sleep(addTime)
    time <- time + addTime
  }
  
  if (tt[3] == "saved") {
    success <- TRUE
    tt <- paste0("Miniconda successfully installed")
  } else if (tt[1] == "Conda already installed at that location"){
    success <- FALSE
    tt <- "Conda already installed at that location"
  }
  
  message(tt)
  return(invisible(success))
})
