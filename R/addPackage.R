# Copyright (c) 2019 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
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
#' @param name Character string.  The name of the package to install.
#' @param filepath Logical. If set to `TRUE`, the name is interpreted as a
#' file path to a local .ssimpkg. Default is `FALSE`.
#' @param session A \code{\link{Session}} object.
#' 
#' @return 
#' This function will invisibly return `TRUE` upon success (i.e.successful 
#' install) and `FALSE` upon failure.
#' 
#' @seealso \link{addPackageFile}
#' 
#' @examples
#' \donttest{
#' temp_dir <- tempdir()
#' mySession <- session()
#' myLibrary <- ssimLibrary(name = file.path(temp_dir,"testlib"), session = mySession)
#' 
#' addPackage("stsim", myLibrary)
#' }
#' 
#' @export
setGeneric("addPackage", function(name, filepath = FALSE, session = NULL) standardGeneric("addPackage"))

#' @rdname addPackage
setMethod("addPackage", signature(session = "character"), function(name, filepath, session) {
  return(SyncroSimNotFound(session))
})

#' @rdname addPackage
setMethod("addPackage", signature(session = "missingOrNULL"), function(name, filepath, session) {
  session <- .session()
  return(addPackage(name, session))
})

#' @rdname addPackage
setMethod("addPackage", signature(session = "Session"), function(name, filepath, session) {
  success <- FALSE
  
  if (is.null(name)) {
    stop("A package name or file path is required")
  }
  
  if (filepath == TRUE) {
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

  packages <- package(session)

  message(tt)
  return(invisible(success))
})
