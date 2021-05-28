# Copyright (c) 2019 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# GPL v.3 License
#' @include AAAClassDefinitions.R
NULL

#' addon(s) of a SsimLibrary or Session
#'
#' Some Packages are Add-On Packages. These Packages can only be used to extend 
#' existing Base Packages; as a result they cannot be used to create new Libraries.
#' For example, stsimsf is an Add-On Package for stsim which provides optional 
#' additional functionality for the base ST-Sim model.
#' More information on add-ons can be found in the 
#' \href{http://docs.syncrosim.com/how_to_guides/package_addon.html}{syncrosim documentation}.
#'
#' The addon(s) of a \code{\link{SsimLibrary}} or \code{\link{Session}}.
#' 
#' @param ssimObject An object of type SsimLibrary/Project/Scenario or Session.
#' 
#' @return 
#' A dataframe listing the addon(s) in use by the library to which the object 
#' belongs.
#' 
#' @examples
#' \donttest{
#' temp_dir <- tempdir()
#' mySession <- session()
#' myLibrary <- ssimLibrary(name = file.path(temp_dir,"testlib"), session = mySession)
#' 
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
