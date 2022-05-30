# Copyright (c) 2021 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Conda configuration of a SsimLibrary
#'
#' Retrieves or sets the Conda configuration of a \code{\link{SsimLibrary}}.
#'
#' @param ssimLibrary \code{\link{SsimLibrary}} object
#' @param value logical for whether to use Conda or not
#' 
#' @return 
#' A logical: the Conda configuration of the SsimLibrary.
#' 
#' @examples 
#' \donttest{
#' # Specify file path and name of new SsimLibrary
#' myLibraryName <- file.path(tempdir(), "testlib")
#' 
#' # Set up a SyncroSim Session, SsimLibrary, Project, and Scenario
#' mySession <- session()
#' myLibrary <- ssimLibrary(name = myLibraryName, session = mySession)
#' 
#' # Retrieve Conda configuration status of the SsimLibrary
#' useConda(myLibrary)
#' 
#' # Set the Conda configuration of the SyncroSim Library
#' useConda(myScenario) <- TRUE
#' }
#' 
#' @export
setGeneric("useConda", function(ssimObject) standardGeneric("useConda"))

#' @rdname useConda
setMethod("useConda", signature(ssimObject = "character"), function(ssimObject) {
  return(SyncroSimNotFound(ssimObject))
})

#' @rdname useConda
setMethod("useConda", signature(ssimObject = "SsimLibrary"), function(ssimObject) {
  cInfo <- info(ssimObject)
  property <- NULL
  return(subset(cInfo, property == "UseConda:")$value)
})

#' @rdname useConda
#' @export
setGeneric("useConda<-", function(ssimObject, value) standardGeneric("useConda<-"))

#' @rdname useConda
setReplaceMethod(
  f = "useConda",
  signature = "character",
  definition = function(ssimObject, value) {
    return(ssimObject)
  }
)

#' @rdname useConda
setReplaceMethod(
  f = "useConda",
  signature = "SsimLibrary",
  definition = function(ssimObject, value) {
    
    if (value == FALSE) {
      tt <- command(list(setprop = NULL, lib = .filepath(ssimObject), useConda = value), .session(ssimObject))
      if (!identical(tt, "saved")) {
        stop(tt)
      }
      return(ssimObject)
    }
    
    if (value == TRUE){
      
      # Check if Conda is installed
      tt <- command(list(conda = NULL, config = NULL))
      if (identical(tt, "No Conda configuration yet")){
        answer <- readline(prompt = "No Conda installation found. Would you like to install MiniConda now? (Y/N)")
        if (answer == "Y"){
          tt <- command(list(setprop = NULL,
                             lib = .filepath(ssimObject),
                             useConda = value), .session(ssimObject))
          if (!identical(tt, "saved")) {
            stop(tt)
          }
          tt <- command(list(conda = NULL, install = NULL))
          if (!identical(tt, "saved")) {
            tt <- command(list(setprop = NULL,
                               lib = .filepath(ssimObject),
                               useConda = FALSE), .session(ssimObject))
            stop(tt)
          }
        } else {
          tt <- command(list(setprop = NULL,
                             lib = .filepath(ssimObject),
                             useConda = FALSE), .session(ssimObject))
          stop("Conda must be installed to use Conda environments.")
        }
      }
      
      # Check if environment needs to be created
      tt <- command()
    }

    
    # old code below for setting name of library
    tt <- command(list(setprop = NULL, lib = .filepath(ssimObject), useConda = value), .session(ssimObject))
    if (!identical(tt, "saved")) {
      stop(tt)
    }
    return(ssimObject)
  }
)
