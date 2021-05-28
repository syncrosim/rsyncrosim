# Copyright (c) 2019 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# GPL v.3 License
#' @include AAAClassDefinitions.R
NULL

#' Retrieves the installed or available packages
#'
#' Retrieves the packages installed or available for this version of SyncroSim.
#'
#' @param session An object of class \code{\link{Session}}.
#' @param installed Logical. `TRUE` to list installed packages and `FALSE` to list 
#' available packages.
#' 
#' @return 
#' Returns a \code{data.frame} of packages installed.
#' 
#' @examples 
#' \donttest{
#' temp_dir <- tempdir()
#' mySession <- session()
#' 
#' package(mySession)
#' }
#' 
#' @export
setGeneric("package", function(session, installed = TRUE) standardGeneric("package"))

#' @rdname package
setMethod("package", signature(session = "missingOrNULL"), function(session, installed = TRUE) {
  session <- .session()
  return(package(session, installed))
})

#' @rdname package
setMethod("package", signature(session = "character"), function(session, installed = TRUE) {
  return(SyncroSimNotFound(session, installed))
})

#' @rdname package
setMethod("package", signature(session = "Session"), function(session, installed = TRUE) {
  arg <- "installed"

  if (!installed) {
    arg <- "available"
  }

  tt <- command(c(arg), session, program = "SyncroSim.PackageManager.exe")

  if (tt[1] == "saved") {
    out <- data.frame(name = NA, displayName = NA, version = NA)
    out <- subset(out, !is.na(name))
  } else if (grepl("The remote name could not be resolved", tt[1])) {
    out <- "Could not connect to the package server."
  } else {
    out <- .dataframeFromSSim(tt, colNames = c("name", "displayName", "version"), csv = FALSE)
  }
  return(out)
})
