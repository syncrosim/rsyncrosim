# Copyright (c) 2024 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Installs Miniforge or Miniconda
#'
#' This function installs the Miniforge or Miniconda package manager software
#' to the default installation path within the SyncroSim installation folder. 
#' If you already have conda installed in the non-default location, you can 
#' point SyncroSim towards that installation using the 
#' \code{\link{condaFilepath}} function.
#'
#' @param session \code{\link{Session-class}} object. If \code{NULL} (default),
#' \code{session()} will be used
#' @param software character. Whether to install the latest release of
#' "miniforge" (Default) or "miniconda".
#' 
#' @return 
#' Invisibly returns \code{TRUE} upon success (i.e.successful 
#' install) and \code{FALSE} upon failure.
#' 
#' @examples
#' \dontrun{
#' # Install miniforge for the default SyncroSim session
#' installConda()
#' 
#' # Install miniconda for the default SyncroSim session
#' installConda(software = "miniconda")
#' }
#' 
#' @export
setGeneric("installConda", 
           function(session, software="miniforge") standardGeneric("installConda"))

#' @rdname installConda
setMethod("installConda", signature(session = "character"), 
          function(session, software) {
  return(SyncroSimNotFound(session))
})

#' @rdname installConda
setMethod("installConda", signature(session = "missingOrNULL"), 
          function(session, software) {
  session <- .session()
  return(installConda(session))
})

#' @rdname installConda
setMethod("installConda", signature(session = "Session"), 
          function(session, software) {
            
  if (software != "miniforge" && software != "miniconda"){
    stop("software must be 'miniforge' or 'miniconda'")
  }
  
  success <- FALSE
  message("Setting conda filepath to the default installation.")
  args <- list(conda = NULL, install = NULL, software = software)

  message("Running conda installer.  Please wait...")
  if (is.null(session)){
    session <- .session()
  }
  tt <- command(args, session)

  if (tt[1] == "Conda is already installed at that location"){
    success <- FALSE
    tt <- "Conda already installed"
  } else if (grepl("successfully installed", tt[3])) {
    success <- TRUE
    tt <- tt[3]
  } 
  
  message(tt)
  
  return(invisible(success))
})
