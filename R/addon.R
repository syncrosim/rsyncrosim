# Copyright (c) 2024 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Addon(s) installed in SsimLibrary or Session
#'
#' Lists the addon SyncroSim package(s) associated with a 
#' \code{\link{SsimLibrary}} or \code{\link{Session}}.
#' These packages can only be used to extend existing SyncroSim base packages; 
#' as a result they cannot be used to create new SsimLibraries.
#' For example, \emph{stsimsf} is an addon for \emph{stsim} which provides optional 
#' additional functionality for the base ST-Sim model.
#' More information on addons can be found in the 
#' \href{https://docs.syncrosim.com/how_to_guides/package_addon.html}{syncrosim documentation}.
#' 
#' @param ssimObject \code{\link{SsimLibrary}} or 
#' \code{\link{Session}} object. If \code{NULL} (default), \code{session()} 
#' will be used
#' 
#' @return 
#' A data.frame listing the addon(s) in use by the SsimLibrary or Session to 
#' which the object belongs.
#' 
#' @examples
#' \donttest{
#' # Install the base package "stsim"
#' addPackage("stsim")
#' 
#' # Set the file path and name of the new SsimLibrary
#' myLibraryName <- file.path(tempdir(),"testlib")
#' 
#' # Set the SyncroSim Session and SsimLibrary
#' mySession <- session()
#' myLibrary <- ssimLibrary(name = myLibraryName, session = mySession)
#' 
#' # Retrieve a data.frame of available add-on(s) for the SsimLibrary
#' addon(myLibrary)
#' }
#' 
#' @export
setGeneric("addon", function(ssimObject) standardGeneric("addon"))

#' @rdname addon
setMethod("addon", signature(ssimObject = "character"), function(ssimObject) {
  return(SyncroSimNotFound(ssimObject))
})

#' @rdname addon
setMethod("addon", signature(ssimObject = "missingOrNULL"), function(ssimObject) {
  ssimObject <- .session()
  tt <- command(list(list = NULL, addons = NULL, csv = NULL), ssimObject)
  tt <- .dataframeFromSSim(tt)
  return(tt)
})

#' @rdname addon
setMethod("addon", signature(ssimObject = "Session"), function(ssimObject) {
  tt <- command(list(list = NULL, addons = NULL, csv = NULL), ssimObject)
  tt <- .dataframeFromSSim(tt)
  return(tt)
})

#' @rdname addon
setMethod("addon", signature(ssimObject = "SsimObject"), function(ssimObject) {
  enabled <- NULL
  tt <- command(list(list = NULL, addons = NULL, csv = NULL, lib = .filepath(ssimObject)), .session(ssimObject))
  tt <- .dataframeFromSSim(tt, convertToLogical = c("enabled"))
  return(tt)
})
