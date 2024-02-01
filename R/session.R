# Copyright (c) 2024 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

# @importFrom utils packageVersion
# @name Session
# @rdname Session-class
setMethod(f = "initialize", signature = "Session", definition = function(.Object, path, silent = FALSE, printCmd = FALSE) {
  
  .Object@filepath <- gsub("\\", "/", gsub("/SyncroSim.Console.exe", "", path, fixed = TRUE), fixed = TRUE)
  .Object@silent <- silent
  .Object@printCmd <- printCmd
  .Object@condaFilepath <- NULL

  ssimRequiredVersion <- "2.4.44"
  ssimCurrentVersion <- command(list(version = NULL), .Object)
  rsyncrosimVersion <- packageVersion("rsyncrosim")
  
  if (!grepl("Version is:", ssimCurrentVersion)) {
    stop("Cannot retrieve SyncroSim version.  At least SyncroSim version 2.1.0 is required.")
  }
  
  ssimCurrentVersion <- gsub("Version is: ", "", ssimCurrentVersion, fixed = TRUE)
  ssimCurrentVersionBits <- as.numeric(strsplit(ssimCurrentVersion, ".", fixed = TRUE)[[1]])
  ssimRequiredVersionBits <- as.numeric(strsplit(ssimRequiredVersion, ".", fixed = TRUE)[[1]])
  
  loadVersion <- FALSE
  if (ssimCurrentVersionBits[1] >= ssimRequiredVersionBits[1]){
    if (ssimCurrentVersionBits[1] > ssimRequiredVersionBits[1]) {
      loadVersion <- TRUE
    } else {
      if (ssimCurrentVersionBits[2] >= ssimRequiredVersionBits[2]){
        if (ssimCurrentVersionBits[2] > ssimRequiredVersionBits[2]) {
          loadVersion <- TRUE
        } else {
          if (ssimCurrentVersionBits[3] >= ssimRequiredVersionBits[3]){
            if (ssimCurrentVersionBits[3] >= ssimRequiredVersionBits[3]){
              loadVersion <- TRUE
            } 
          }
        }
      } 
    }
  }
  
  if (!loadVersion) {
    stop(paste0("SyncroSim v", ssimRequiredVersion,
                " is required to run rsyncrosim v", rsyncrosimVersion,
                " but you have SyncroSim v", ssimCurrentVersion, " installed."))
  }
  return(.Object)
})

#' Create or return SyncroSim Session
#'
#' Methods to create or return a SyncroSim \code{\link{Session}}.
#' 
#' @param x character or SsimObject. Path to SyncroSim installation. If \code{NULL}
#' (default), then default path is used
#' @param silent logical. Applies only if x is a path or \code{NULL} If \code{TRUE}, warnings 
#'     from the console are ignored. Otherwise they are printed. Default is \code{FALSE}
#' @param printCmd logical. Applies only if x is a path or \code{NULL} If \code{TRUE}, 
#'     arguments passed to the SyncroSim console are also printed. Helpful for 
#'     debugging. Default is \code{FALSE}
#' @param ssimObject \code{\link{Project}} or \code{\link{Scenario}} object
#' @param value \code{\link{Session}} object
#' 
#' @details
#'
#' In order to avoid problems with SyncroSim version compatibility and SsimLibrary 
#' updating, the new Session must have the same filepath as the Session of the 
#' SsimObject 
#' e.g. \code{filepath(value)==filepath(session(ssimObject))}.
#' Therefore, the only time when you will need to set a new SyncroSim Session is if you 
#' have updated the SyncroSim software and want to update an existing SsimObject
#' to use the new software.
#' 
#' @return 
#' A SyncroSim \code{\link{Session}} object.
#' 
#' @examples
#' \dontrun{
#' # Specify file path and name of new SsimLibrary
#' myLibraryName <- file.path(tempdir(), "testlib")
#' 
#' # Set up a SyncroSim Session, SsimLibrary, and Project
#' mySession <- session()
#' myLibrary <- ssimLibrary(name = myLibraryName, session = mySession)
#' myProject <- project(myLibrary, project = "Definitions")
#'
#' # Lists the folder location of SyncroSim Session
#' filepath(mySession)
#' 
#' # Lists the version of SyncroSim Session
#' version(mySession)
#' 
#' # Data frame of the packages installed with this version of SyncroSim
#' package(mySession) 
#' 
#' # Data frame of the base packages installed with this version of SyncroSim
#' package(mySession, installed = "BASE") 
#' 
#' # Set a new SyncroSim Session for the SyncroSim Project
#' session(myProject) <- session(x = filepath(session(myProject)))
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
  
  if (endsWith(path, "/SyncroSim.Console.exe")) {
    progName <- dirname(path)
  } else {
    progName <- path
  }

  return(new("Session", path, silent, printCmd))
})

#' @rdname session
setMethod("session", signature(x = "SsimObject"), function(x, silent, printCmd) x@session)

#' @rdname session
setMethod("session", signature(x = "Folder"), function(x, silent, printCmd) x@session)

#' @rdname session
#' @export
setGeneric("session<-", function(ssimObject, value) standardGeneric("session<-"))

#' @rdname session
setReplaceMethod(
  f = "session",
  signature = "NULLOrChar",
  definition = function(ssimObject, value) {
    return(ssimObject)
  }
)

#' @rdname session
setReplaceMethod(
  f = "session",
  signature = "SsimObject",
  definition = function(ssimObject, value) {
    if (!is(value, "Session")) {
      stop("Must assign a Session object.")
    }
    if (.filepath(value) != .filepath(.session(ssimObject))) {
      stop("The new session must have the same filepath as the session of the SsimObject e.g. filepath(value)==filepath(session(ssimObject))")
    }
    ssimObject@session <- value
    return(ssimObject)
  }
)
