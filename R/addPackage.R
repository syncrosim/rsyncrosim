# Copyright (c) 2024 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Adds package to SyncroSim Installation
#'
#' This function installs a package to the SyncroSim \code{\link{Session}}.
#' If only the package name is provided as input, the function queries the 
#' SyncroSim package server for the specified package. If a file path is 
#' provided as input, the function adds a package to SyncroSim from a local 
#' package file (ends in ".ssimpkg"). The list of SyncroSim packages can be 
#' found \href{https://syncrosim.com/packages/}{here}.
#'
#' @param name character string.  The name or file path of the package to 
#' install
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
#' # Add package from the package server
#' addPackage("stsim", session = mySession)
#' 
#' # Add package using a local file path
#' addPackage("c:/path/to/stsim.ssimpkg")
#' }
#' 
#' @export
setGeneric("addPackage", function(name, session = NULL) standardGeneric("addPackage"))

#' @rdname addPackage
setMethod("addPackage", signature(session = "character"), function(name, session) {
  return(SyncroSimNotFound(session))
})

#' @rdname addPackage
setMethod("addPackage", signature(session = "missingOrNULL"), function(name, session) {
  session <- .session()
  return(addPackage(name, session))
})

#' @rdname addPackage
setMethod("addPackage", signature(session = "Session"), function(name, session) {
  success <- FALSE
  
  if (is.null(name)) {
    stop("A package name or file path is required")
  }
  
  if (grepl(".ssimpkg", name)) {
    if (!file.exists(name)) {
      tt <- paste0("Cannot find file: ", name)
    } else{
      tt <- command(args = list(finstall = name), session, program = "SyncroSim.PackageManager.exe")
      if (tt == "saved"){
        success <- TRUE
        tt <- paste0("Package installed from file <", name, ">")
      }
    }
  } else {
    packages <- package(session)
    if (is.element(name, packages$name)) {
      tt <- (paste0("Package <", name, "> is already installed"))
    } else {
      tt <- command(args = list(install = name), session, program = "SyncroSim.PackageManager.exe")
      if (tt == "saved"){
        tt <- paste0("Package <", name, "> installed")
        success <- TRUE
      }
    }
  }

  message(tt)
  return(invisible(success))
})
