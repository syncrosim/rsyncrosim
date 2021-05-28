# Copyright (c) 2019 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# GPL v.3 License
#' @include AAAClassDefinitions.R
NULL

#' Update Package
#'
#' Updates a SyncroSim package.
#'
#' @param name Character string.  The name of the package to update. 
#'     If NULL, all packages will be updated.
#' @param session An object of class \code{\link{Session}}.
#' @param listonly Logical. If TRUE, available updates are listed only.
#' 
#' @return 
#' This function invisibly returns `TRUE` upon success (i.e.successful update)
#' and `FALSE` upon failure.
#' 
#' @examples 
#' \donttest{
#' temp_dir <- tempdir()
#' mySession <- session()
#' 
#' updatePackage(name = "stsim", session = mySession, listonly = FALSE)
#' updatePackage(name = "stsim", session = mySession, listonly = TRUE)
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
