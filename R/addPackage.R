# Copyright (c) 2019 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# GPL v.3 License
#' @include AAAClassDefinitions.R
NULL

#' Adds a package to SyncroSim
#'
#' Adds a package to SyncroSim. This functions will query the syncrosim 
#' package server for the package name provided as input.
#'
#' @param name Character string.  The name of the package to install.
#' @param session Session.
#' 
#' @return 
#' This function will invisibly return `TRUE` upon success (i.e.successful 
#' install) and `FALSE` upon failure.
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
