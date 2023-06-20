# Copyright (c) 2023 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Retrieve folder data for a SsimLibrary
#'
#' Retrieve a table of folder data for a \code{\link{SsimLibrary}}. 
#' The folder data consists of folder names and corresponding folder IDs
#' that will be used to organize Scenarios in the SyncroSim User Interface.
#'
#' @param ssimLibrary \code{\link{SsimLibrary}}
#' 
#' @return 
#' Returns a \code{data.frame} with information about project folders in the 
#' SsimLibrary 
#' 
#' @examples
#' \donttest{
#' # Specify file path and name of new SsimLibrary
#' myLibraryName <- file.path(tempdir(), "testlib")
#' 
#' # Set up a SyncroSim Session and SsimLibrary
#' mySession <- session()
#' myLibrary <- ssimLibrary(name = myLibraryName, session = mySession)
#' 
#' # Retrieve folder data from the SsimLibrary
#' retrieveFolderData(myLibrary)
#' }
#' 
#' @export
setGeneric("retrieveFolderData", function(ssimLibrary) standardGeneric("retrieveFolderData"))

#' @rdname retrieveFolderData
setMethod("retrieveFolderData", signature(ssimLibrary = "character"), function(ssimLibrary) {
  return(SyncroSimNotFound(ssimLibrary))
})

#' @rdname retrieveFolderData
setMethod("retrieveFolderData", signature(ssimLibrary = "SsimObject"), function(ssimLibrary) {
  args <- list(lib = .filepath(ssimLibrary), list = NULL, folders = NULL)
  tt <- command(args = args, session = .session(ssimLibrary))
  out <- .dataframeFromSSim(tt, localNames = TRUE, csv=FALSE)
  return(out)
})
