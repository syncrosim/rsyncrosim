# Copyright (c) 2021 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Installs Miniconda
#'
#' This function installs Miniconda to the default installation path
#' within the SyncroSim installation folder. If you already have Conda 
#' installed in the non-default location, you can point SyncroSim towards
#' that installation using the \code{\link{condaFilepath}} function.
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
#' # Install Conda for the default SyncroSim session
#' installConda()
#' }
#' 
#' @export
setGeneric("installConda", function(session) standardGeneric("installConda"))

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
  message("Setting Conda filepath to the default installation.")
  args <- list(conda = NULL, install = NULL)

  message("Running Conda Installer.  Please wait...")
  if (is.null(session)){
    session <- .session()
  }
  tt <- command(args, session)

  if (tt[1] == "Conda already installed at that location"){
    success <- FALSE
    tt <- "Conda already installed"
  } else if (tt[3] == "Saved") {
    success <- TRUE
    tt <- paste0("Miniconda successfully installed")
  } 
  
  message(tt)
  
  return(invisible(success))
})
