# Copyright (c) 2024 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Conda configuration of a SsimLibrary
#'
#' Retrieves or sets the Conda configuration of a \code{\link{SsimLibrary}}. Note
#' that in order to use conda environments, you will first need to ensure that
#' the conda environment has been created for a given package. You can create 
#' the conda environment for a package using the \code{\link{createCondaEnv}}
#' function.
#'
#' @param ssimObject \code{\link{SsimLibrary}} object
#' @param value logical for whether to use Conda 
#' environments for the given SyncroSim Library. If set to 
#' \code{TRUE}, then Conda environments will be used. If set to \code{FALSE},
#' then Conda environments will not be used during runtime.
#' 
#' @return 
#' Logical: whether Conda environments will be used during runtime for the given
#'  \code{\link{SsimLibrary}}
#' 
#' @examples
#' \dontrun{
#' # Set up a SyncroSim Session, SsimLibrary
#' mySession <- session()
#' 
#' # Retrieve Conda configuration status of the SsimLibrary
#' useConda(myLibrary)
#' 
#' # Set the Conda configuration of the SyncroSim Library
#' useConda(myLibrary) <- TRUE
#' 
#' # Only use Conda with the specified SyncroSim packages
#' useConda(myLibrary) <- "helloworld"
#' 
#' # Only use Conda with multiple specified SyncroSim packages
#' useConda(myLibrary) <- c("helloworld", "stsim")
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
  useCondaValue <- subset(cInfo, property == "Use Conda:")$value
  
  if (useCondaValue == "Yes") {
    return(TRUE)
  } else {
    return(FALSE)
  }
})

#' @rdname useConda
#' @export
setGeneric("useConda<-", function(ssimObject, value) standardGeneric("useConda<-"))

#' @rdname useConda
setReplaceMethod(
  f = "useConda",
  signature = "logical",
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
      
      tt <- command(list(setprop = NULL, lib = .filepath(ssimObject), useconda = "no"), .session(ssimObject))
      if (!identical(tt, "saved")) {
        stop(tt)
      }
      
      return(ssimObject)
    }
    
    if (value == TRUE){
      
      tt <- command(list(setprop = NULL, lib = .filepath(ssimObject), 
                         useconda = "yes"), .session(ssimObject))
    }
    
    return(ssimObject)
  }
)
