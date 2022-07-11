# Copyright (c) 2021 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Installs Miniconda
#'
#' This function installs Miniconda to the default installation path
#' within the SyncroSim installation folder.
#'
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
#' installConda(mySession)
#' }
#' 
#' @export
setGeneric("installConda", function(session = NULL) standardGeneric("installConda"))

#' @rdname installConda
setMethod("installConda", signature(session = "character"), function(session) {
  return(SyncroSimNotFound(session))
})

#' @rdname installConda
setMethod("installConda", signature(session = "missingOrNULL"), function(session) {
  session <- .session()
  return(installConda(session))
})

#' @rdname installConda
setMethod("installConda", signature(session = "Session"), function(session) {
  success <- FALSE

  args <- list(conda = NULL, install = NULL)
  
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
    tt <- "Conda already installed"
  }
  
  message(tt)
  return(invisible(success))
})
