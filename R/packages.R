# Copyright (c) 2024 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Installed or available packages
#'
#' Retrieves the packages installed or available in the current session if 
#' called on a \code{\link{Session}} object, or the packages added to a 
#' SyncroSim Library if called on a \code{\link{SsimLibrary}} object.
#'
#' @param ssimObject \code{\link{Session}} or 
#' \code{\link{SsimLibrary}} object. If \code{NULL} (default), \code{session()}
#' will be used
#' @param installed logical or character. \code{TRUE} (default) to list 
#' installed packages or \code{FALSE} to list available packages on the server
#' 
#' @return 
#' Returns a \code{data.frame} of packages installed or templates available 
#' for a specified package.
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
#' packages(mySession)
#' 
#' # List all available packages on the server (including currently installed)
#' packages(installed = FALSE)
#'  
#' # Check the package(s) in your SsimLibrary
#' packages(myLibrary)
#' }
#' 
#' @export
setGeneric("packages", 
           function(ssimObject = NULL, installed = TRUE) standardGeneric("packages"))

#' @rdname packages
setMethod("packages", signature(ssimObject = "character"), 
          function(ssimObject, installed = TRUE) {
  return(SyncroSimNotFound(ssimObject, installed))
})

#' @rdname packages
setMethod("packages", signature(ssimObject = "missingOrNULL"), 
          function(ssimObject, installed = TRUE) {
  ssimObject <- .session()
  return(packages(ssimObject, installed))
})

#' @rdname packages
setMethod("packages", signature(ssimObject = "Session"), 
          function(ssimObject, installed = TRUE) {
            
    Name <- NULL 
    
    if (installed == FALSE) {
      arg <- "available"
    } else {
      arg <- "installed"
    }
    
    if (is.logical(installed)) {
      
      tt <- command(c(arg), ssimObject, program = "SyncroSim.PackageManager.exe")
      
      if (tt[1] == "saved") {
        out <- subset(out, !is.na(Name))
      } else if (grepl("The remote name could not be resolved", tt[1])) {
        out <- "Could not connect to the package server."
      } else {
        out <- .dataframeFromSSim(tt, localNames = TRUE, csv=FALSE)
      }
      
      drops <- c("x")
      out <- out[ , !(names(out) %in% drops)]
      
      return(out)
      
    } else {
      
      tt <- command(c("list", arg, "csv"), ssimObject)
      out <- .dataframeFromSSim(tt, localNames = TRUE, csv = FALSE)
      drops <- c("x")
      out <- out[ , !(names(out) %in% drops)]
      
      return(out)
    }
})

#' @rdname packages
setMethod("packages", signature(ssimObject = "SsimLibrary"), function(ssimObject) {
  
  # Retrieve list of packages in library
  args <- list(list = NULL, packages = NULL, lib = filepath(ssimObject), csv = NULL)
  tt <- command(args, .session(ssimObject), program = "SyncroSim.Console.exe")
  out <- .dataframeFromSSim(tt, csv = T)
  
  return(out)
})
