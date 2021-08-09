# Copyright (c) 2021 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Installed or available packages
#'
#' Retrieves the packages installed or available for this version of SyncroSim.
#'
#' @param ssimObject \code{\link{Session}} or 
#' \code{\link{SsimLibrary}} object. If \code{NULL} (default), \code{session()}
#' will be used
#' @param installed logical or character. \code{TRUE} (default) to list installed packages, 
#' \code{FALSE} to list available packages, and "BASE" to list installed base 
#' packages
#' 
#' @return 
#' Returns a \code{data.frame} of packages installed.
#' 
#' @examples 
#' \donttest{
#' # Set the file path and name of the new SsimLibrary
#' myLibraryName <- file.path(tempdir(),"testlib")
#' 
#' # Set the SyncroSim Session and SsimLibrary
#' mySession <- session()
#' myLibrary <- ssimLibrary(name = myLibraryName, session = mySession)
#' 
#' # List all installed packages
#' package(mySession)
#' 
#' # List all the installed base packages
#' package(installed = "BASE")
#' 
#' # List all available packages on the server (including currently installed)
#' package(installed = FALSE)
#'  
#' # Check the package you're SsimLibrary is currently using
#' package(myLibrary)
#' }
#' 
#' @export
setGeneric("package", function(ssimObject = NULL, installed = TRUE) standardGeneric("package"))

#' @rdname package
setMethod("package", signature(ssimObject = "character"), function(ssimObject, installed = TRUE) {
  return(SyncroSimNotFound(ssimObject, installed))
})

#' @rdname package
setMethod("package", signature(ssimObject = "missingOrNULL"), function(ssimObject, installed = TRUE) {
  ssimObject <- .session()
  return(package(ssimObject, installed))
})

#' @rdname package
setMethod("package", signature(ssimObject = "Session"), function(ssimObject, installed = TRUE) {
  arg <- "installed"

  if (installed == FALSE) {
    arg <- "available"
  }
  
  if (installed == "BASE") {
    arg <- "basepkgs"
  }
  
  if (is.logical(installed)) {
    tt <- command(c(arg), ssimObject, program = "SyncroSim.PackageManager.exe")
    
    if (tt[1] == "saved") {
      out <- data.frame(name = NA, displayName = NA, version = NA)
      out <- subset(out, !is.na(name))
    } else if (grepl("The remote name could not be resolved", tt[1])) {
      out <- "Could not connect to the package server."
    } else {
      out <- .dataframeFromSSim(tt, colNames = c("name", "description", "version"), csv = FALSE)
    }
    return(out)
  } else {
    tt <- command(c("list", arg, "csv"), ssimObject)
    out <- .dataframeFromSSim(tt, localNames = TRUE, csv = FALSE)
    return(out)
  }
})

#' @rdname package
setMethod("package", signature(ssimObject = "SsimLibrary"), function(ssimObject) {
  oInf <- info(ssimObject)
  property <- NULL
  out <- data.frame(name = subset(oInf, property == "Package Name:")$value)
  out$description <- subset(oInf, property == "Package Description:")$value
  out$version <- subset(oInf, property == "Current Package Version:")$value
  return(out)
})
