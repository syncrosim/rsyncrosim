# Copyright (c) 2019 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# GPL v.3 License
#' @include AAAClassDefinitions.R
NULL

# @name Session
# @rdname Session-class
setMethod(f = "initialize", signature = "Session", definition = function(.Object, path, silent = FALSE, printCmd = FALSE) {
  .Object@filepath <- gsub("\\", "/", gsub("/SyncroSim.Console.exe", "", path, fixed = TRUE), fixed = TRUE)
  .Object@silent <- silent
  .Object@printCmd <- printCmd

  vs <- command(list(version = NULL), .Object)

  if (!grepl("Version is:", vs)) {
    stop("Cannot retrieve SyncroSim version.  At least SyncroSim version 2.1.0 is required.")
  }

  vs <- gsub("Version is: ", "", vs, fixed = TRUE)
  vs <- as.numeric(strsplit(vs, ".", fixed = TRUE)[[1]])

  if (vs[1] < 2) {
    stop("rsyncrosim requires at least SyncroSim version 2.1.0.")
  }

  if (vs[2] < 1) {
    stop("rsyncrosim requires at least SyncroSim version 2.1.0.")
  }
  return(.Object)
})

#' Creates or returns a SyncroSim Session.
#'
#' Methods to create or return a SyncroSim \code{\link{Session}}.
#' 
#' @param x Character or SsimObject. An optional path to the SyncroSim installation.
#' @param silent Logical. Applies only if x is a path or NULL. If TRUE, warnings from the console are ignored. Otherwise they are printed.
#' @param printCmd Logical. Applies only if x is a path or NULL. If TRUE, arguments passed to the SyncroSim console are also printed. Helpful for debugging. FALSE by default.
#' 
#' @return 
#' A SyncroSim \code{\link{Session}} object.
#' 
#' @examples
#' \donttest{
#' # Create Session
#' temp_dir <- tempdir()
#' mySession <- session()
#' myLibrary <- ssimLibrary(name = file.path(temp_dir,"testlib"), session = mySession)
#'
#' filepath(mySession) # Lists the folder location of SyncroSim session
#' version(mySession) # Lists the version of SyncroSim session
#' package(mySession) # Dataframe of the packages installed with this version of SyncroSim
#' basePackage(mySession) # Dataframe of the base packages installed with this version of SyncroSim
#' }
#' 
#' @export
setGeneric("session", function(x = NULL, silent = TRUE, printCmd = FALSE) standardGeneric("session"))

#' @rdname session
setMethod("session", signature(x = "missingOrNULLOrChar"), function(x, silent, printCmd) {
  path <- x

  if (!is.null(path)) {
    if (!grepl("SyncroSim.Console.exe", path, fixed = TRUE)) {
      path <- paste0(path, "/SyncroSim.Console.exe")
    }

    if (!file.exists(path)) {
      warning(paste("SyncroSim console could not be found at:", path))
      return(SyncroSimNotFound(warn = FALSE))
    }
  } else {
    e <- ssimEnvironment()
    path <- e$ProgramDirectory

    if (is.na(path) || !dir.exists(path)) {
      if (.Platform$OS.type == "windows") {
        envVars <- Sys.getenv(c("PROGRAMFILES", "ProgramW6432"), names = FALSE)
        envVars <- envVars[envVars != ""]

        for (i in seq(length.out = length(envVars))) {
          cPath <- paste0(envVars[i], "\\SyncroSim")
          if (file.exists(paste0(cPath, "\\SyncroSim.Console.exe"))) {
            path <- cPath
            break
          }
        }
      }
      else {
        path <- dirname(Sys.which("SyncroSim.Console.exe"))
        
        if (is.null(path) || (!dir.exists(path))){
          path = paste(path.expand("~"), "SyncroSim", sep='/')
        }
        
        if (!file.exists(paste(path, "/SyncroSim.Console.exe", sep="/"))) {
          path = NULL;
        }
      }

      if (is.null(path) || (!dir.exists(path))) {
        path <- NULL
      }
    }
  }

  if (is.null(path)) {
    warning("Default SyncroSim installation not found. Either install SyncroSim in the default location, or explicitly set the session path. See ?session for details.")
    return(SyncroSimNotFound(warn = FALSE))
  }

  return(new("Session", path, silent, printCmd))
})

#' @rdname session
setMethod("session", signature(x = "SsimObject"), function(x, silent, printCmd) x@session)

#' Set a SyncroSim Session.
#'
#' Set the Session of a \code{\link{SsimLibrary}}, \code{\link{Project}} or \code{\link{Scenario}} object.
#'
#' @details
#'
#' In order to avoid problems with SyncroSim version compatibility and library updating,
#' the new session must have the same filepath as the session of the SsimObject e.g. filepath(value)==filepath(session(ssimObject))
#'
#' @param ssimObject SsimObject, \code{\link{Project}} or \code{\link{Scenario}}.
#' @param value A SyncroSim \code{\link{Session}}.
#' 
#' @return 
#' Returns a SyncroSim object containing a Session.
#' 
#' @export
setGeneric("session<-", function(ssimObject, value) standardGeneric("session<-"))

#' @rdname session-set
setReplaceMethod(
  f = "session",
  signature = "character",
  definition = function(ssimObject, value) {
    return(ssimObject)
  }
)

#' @rdname session-set
setReplaceMethod(
  f = "session",
  signature = "SsimObject",
  definition = function(ssimObject, value) {
    if (class(value) != "Session") {
      stop("Must assign a Session object.")
    }
    if (.filepath(value) != .filepath(.session(ssimObject))) {
      stop("The new session must have the same filepath as the session of the SsimObject e.g. filepath(value)==filepath(session(ssimObject))")
    }
    ssimObject@session <- value
    return(ssimObject)
  }
)
