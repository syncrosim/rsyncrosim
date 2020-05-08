# Copyright (c) 2019 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# GPL v.3 License
#' @include AAAClassDefinitions.R
NULL

#' Adds a package to SyncroSim
#'
#' Adds a package to SyncroSim.
#'
#' @param name Character string.  The name of the package to install from the online package server.
#' @param session Session.
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
  if (is.null(name)) {
    stop("A package name is required.")
  }

  packages <- package(session)

  if (is.element(name, packages$name)) {
    tt <- (paste0("Package <", name, "> is already installed"))
  } else {
    tt <- command(args = list(install = name), session, program = "SyncroSim.PackageManager.exe")
    if (tt == "saved"){
      tt <- paste0("Package <", name, "> installed.")
    }
  }
  message(tt)
})
