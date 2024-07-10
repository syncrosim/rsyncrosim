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
#' @param listTemplates character. Name of a SyncroSim package. If not \code{NULL} 
#' (default), then lists all templates available for that package. The package
#' must be installed in the current Session. Ignored if ssimObject is a 
#' \code{\link{SsimLibrary}} object
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
#' # List all the installed base packages
#' packages(installed = "BASE")
#' 
#' # List all available packages on the server (including currently installed)
#' packages(installed = FALSE)
#'  
#' # Check the package(s) in your SsimLibrary
#' packages(myLibrary)
#' 
#' # Check the templates available for an installed package
#' installPackage("helloworldSpatial")
#' packages(listTemplates = "helloworldSpatial")
#' }
#' 
#' @export
setGeneric("packages", function(ssimObject = NULL, installed = TRUE, listTemplates = NULL) standardGeneric("packages"))

#' @rdname packages
setMethod("packages", signature(ssimObject = "character"), function(ssimObject, installed = TRUE, listTemplates) {
  return(SyncroSimNotFound(ssimObject, installed))
})

#' @rdname packages
setMethod("packages", signature(ssimObject = "missingOrNULL"), function(ssimObject, installed = TRUE, listTemplates) {
  ssimObject <- .session()
  return(packages(ssimObject, installed, listTemplates))
})

#' @rdname packages
setMethod("packages", signature(ssimObject = "Session"), function(ssimObject, installed = TRUE, listTemplates) {
  if (is.null(listTemplates)) {
    arg <- "installed"
  
    if (installed == FALSE) {
      arg <- "available"
    }
    
    if (is.logical(installed)) {
      tt <- command(c(arg), ssimObject, program = "SyncroSim.PackageManager.exe")
      
      if (tt[1] == "saved") {
        # out <- data.frame(name = NA, displayName = NA, version = NA)
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
  } else {
    if (is.character(listTemplates)) {
      
      # Make sure package is installed
      pkgList <- command(c("installed"), ssimObject,
                         program = "SyncroSim.PackageManager.exe")
      pkgDf <- .dataframeFromSSim(pkgList,
                                  localNames = TRUE,
                                  csv = FALSE)
      if (listTemplates %in% pkgDf$name == FALSE) {
        stop("SyncroSim package not installed")
      }
      
      # Retrieve list of templates
      args <- list(list = NULL, templates = NULL, noheaders = NULL,
                   package = listTemplates)
      tt <- command(args, program = "SyncroSim.Console.exe")
      out <- .dataframeFromSSim(tt,
                                colNames =c("name", "displayName", "installed"),
                                csv = F)
      drops <- c("x")
      out <- out[ , !(names(out) %in% drops)]
      return(out)
    } else {
      stop("listTemplates must be a character name of a SyncroSim Package")
    }
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
