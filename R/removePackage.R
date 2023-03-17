# Copyright (c) 2023 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Removes package from SyncroSim installation
#' 
#' @param name character. The name of the package to remove
#' @param session \code{\link{Session}} object. If \code{NULL} (default), 
#' \code{session()} will be used
#' @param force logical. If \code{TRUE}, remove without requiring confirmation from 
#'     the user. Default is \code{FALSE}
#' 
#' @return 
#' Invisibly returns \code{TRUE} upon success (i.e.successful 
#' removal) and \code{FALSE} upon failure.
#' 
#' @examples 
#' \donttest{
#' # Set SyncroSim Session
#' mySession <- session()
#' 
#' # Remove package from SyncroSim Session
#' removePackage("stsim", mySession, force = FALSE)
#' }
#' 
#' @export
setGeneric("removePackage", function(name, session = NULL, force = FALSE) standardGeneric("removePackage"))

#' @rdname removePackage
setMethod("removePackage", signature(session = "character"), function(name, session, force) {
  return(SyncroSimNotFound(session))
})

#' @rdname removePackage
setMethod("removePackage", signature(session = "missingOrNULL"), function(name, session, force) {
  session <- .session(session)
  return(removePackage(name, session, force))
})

#' @rdname removePackage
setMethod("removePackage", signature(session = "Session"), function(name, session, force) {
  installed <- package(session)
  success <- FALSE
  
  if (!is.element(name, installed$name)) {
    stop("The package is not installed.")
  }
  
  if (force) {
    answer <- "y"
  } else {
    answer <- readline(prompt = paste0("Do you really want to remove package '", name, "'? (y/n)"))
  }
  
  if (answer == "y") {
    tt <- command(args = list(uninstall = name), session, program = "SyncroSim.PackageManager.exe")
    if (tt == "saved"){
      tt <- paste0("Package <", name,"> removed")
      success <- TRUE
    } 
  } else {
    tt <- paste0("Removal of package <", name,"> skipped")
  }
  message(tt)
  return(invisible(success))
})
