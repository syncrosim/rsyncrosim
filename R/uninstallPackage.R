# Copyright (c) 2023 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Removes a package from SyncroSim installation
#' 
#' @param name character. The name of the package to uninstall
#' @param session \code{\link{Session}} object. If \code{NULL} (default), 
#' \code{session()} will be used
#' @param force logical. If \code{TRUE}, uninstall without requiring 
#'     confirmation from the user. Default is \code{FALSE}
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
#' # Uninstalls package from SyncroSim Session
#' uninstallPackage("stsim", mySession, force = FALSE)
#' }
#' 
#' @export
setGeneric("uninstallPackage", function(name, session = NULL, force = FALSE) standardGeneric("uninstallPackage"))

#' @rdname uninstallPackage
setMethod("uninstallPackage", signature(session = "character"), function(name, session, force) {
  return(SyncroSimNotFound(session))
})

#' @rdname uninstallPackage
setMethod("uninstallPackage", signature(session = "missingOrNULL"), function(name, session, force) {
  session <- .session(session)
  return(uninstallPackage(name, session, force))
})

#' @rdname uninstallPackage
setMethod("uninstallPackage", signature(session = "Session"), function(name, session, force) {
  installed <- packages(session)
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
