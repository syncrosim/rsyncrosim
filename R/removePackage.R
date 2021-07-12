# Copyright (c) 2019 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# GPL v.3 License
#' @include AAAClassDefinitions.R
NULL

#' Removes a package from your SyncroSim installation.
#' 
#' @param name Character. The name of the package to delete.
#' @param session An object of class \code{\link{Session}}.
#' @param force Logical. If TRUE, delete without requiring confirmation from 
#'     the user.
#' 
#' @return 
#' This function invisibly returns `TRUE` upon success (i.e.successful 
#' removal) and `FALSE` upon failure.
#' 
#' @examples 
#' \donttest{
#' temp_dir <- tempdir()
#' mySession <- session()
#' 
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
