# Copyright (c) 2021 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Update Package
#'
#' Updates a SyncroSim package.
#'
#' @param name character string.  The name of the package to update. 
#'     If \code{NULL} (default), all packages will be updated
#' @param session \code{\link{Session}} object. If \code{NULL} (default), 
#' \code{session()} is used
#' @param listonly logical. If \code{TRUE}, available updates are listed only. 
#' Default is \code{FALSE}
#' 
#' @return 
#' Invisibly returns \code{TRUE} upon success (i.e.successful update)
#' and \code{FALSE} upon failure.
#' 
#' @examples 
#' \donttest{
#' # Set SyncroSim Session
#' mySession <- session()
#' 
#' # List all available updates for a package
#' updatePackage(name = "stsim", session = mySession, listonly = TRUE)
#' 
#' # Update ST-Sim package
#' updatePackage(name = "stsim", session = mySession, listonly = FALSE)
#' 
#' # Update all packages
#' updatePackage(session = mySession)
#' }
#' 
#' @export
setGeneric("updatePackage", function(name = NULL, session = NULL, listonly = FALSE) standardGeneric("updatePackage"))

#' @rdname updatePackage
setMethod("updatePackage", signature(session = "character"), function(name, session, listonly) {
  return(SyncroSimNotFound(session))
})

#' @rdname updatePackage
setMethod("updatePackage", signature(session = "missingOrNULL"), function(name, session, listonly) {
  session <- .session()
  return(updatePackage(name, session, listonly))
})

#' @rdname updatePackage
setMethod("updatePackage", signature(session = "Session"), function(name, session, listonly) {
  success <- FALSE
  
  if (listonly) {
    tt <- command(args = "--updates", session, program = "SyncroSim.PackageManager.exe", )
    if (is.na(tt[1])){
      message("Could not connect to the package server.") # tt is NA du to connection error
    }
    return(invisible(success))
  }
  
  tt <- NULL
  
  if (is.null(name)) {
    answer <- readline(prompt = "Update all packages? (y/n)")
    
    if (answer == "y") {
      tt <- command(args = "--updateall --force", session, program = "SyncroSim.PackageManager.exe")
    } else {
      message("Update process skipped")
      return(invisible(success))
    }
  } else {
    installed <- package(session)
    
    if (!is.element(name, installed$name)) {
      message(paste(name, ": The package is not installed."))
      return(invisible(success))
    } else {
      tt <- command(args = list(updatepkg = name), session, program = "SyncroSim.PackageManager.exe")
      if (!is.na(tt[1])){
        if (tt == "saved"){
          success <- TRUE
        } else {
          message(tt)
        }
      } else{
        message("Could not connect to the package server.") # tt is NA du to connection error
      }
    }
  }
  
  return(invisible(success))
})
