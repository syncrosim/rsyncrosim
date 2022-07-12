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
#' # Install Conda for the default SyncroSim session
#' installConda()
#' }
#' 
#' @export
installConda <- function() {
  
  success <- FALSE
  message("Setting Conda filepath to the default installation.")
  args <- list(conda = NULL, install = NULL)

  message("Running Conda Installer.  Please wait...")
  session <- .session()
  tt <- command(args, session)

  
  if (tt[3] == "Saved") {
    success <- TRUE
    tt <- paste0("Miniconda successfully installed")
  } else if (tt[1] == "Conda already installed at that location"){
    success <- FALSE
    tt <- "Conda already installed"
  }
  
  message(tt)
  
  return(invisible(success))
}
