# Copyright (c) 2021 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# GPL v.3 License
#' @include AAAClassDefinitions.R
NULL

#' Adds a package to SyncroSim
#'
#' This function adds a package to SyncroSim. If only the package name is 
#' provided as input, the function queries the  SyncroSim package server for 
#' the package name provided as input. If a file path is given as input, the 
#' function adds a package to SyncroSim from a local package file (ends in 
#' .ssimpkg). The list of SyncroSim packages can be found 
#' \href{https://syncrosim.com/packages/}{here}.
#'
#' @param name Character string.  The name or file path of the package to 
#' install.
#' @param session A \code{\link{Session}} object.
#' 
#' @return 
#' This function will invisibly return `TRUE` upon success (i.e.successful 
#' install) and `FALSE` upon failure.
#' 
#' @examples
#' \donttest{
#' temp_dir <- tempdir()
#' mySession <- session()
#' 
#' # Add package from the package server
#' addPackage("stsim", session = mySession)
#' }
#' \dontrun{
#' temp_dir <- tempdir()
#' mySession <- session()
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
