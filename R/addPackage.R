# Copyright (c) 2019 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# GPL v.3 License
#' @include AAAClassDefinitions.R
NULL

#' Adds a package to SyncroSim
#'
#' This function adds a package to SyncroSim. The function first queries the 
#' SyncroSim package server for the package name provided as input. The list of 
#' SyncroSim packages can be found \href{https://syncrosim.com/packages/}{here}.
#'
#' @param name Character string.  The name of the package to install.
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
#' myses <- session()
#' myLibrary <- ssimLibrary(name = file.path(temp_dir,"testlib"), session = myses)
#' 
#' addPackage(myLibrary, "stsim")
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
    stop("A package name is required")
  }

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
  message(tt)
  return(invisible(success))
})
