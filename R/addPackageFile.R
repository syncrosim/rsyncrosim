# Copyright (c) 2021 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# GPL v.3 License
#' @include AAAClassDefinitions.R
NULL

#' Adds a package to SyncroSim
#'
#' This function is now deprecated. See: \code{\link{addPackage}}.
#'
#' @param filename Character string.  The path to a SyncroSim package file.
#' @param session A \code{\link{Session}} object.
#' 
#' @return 
#' This function invisibly returns `TRUE` upon success (i.e.successful 
#' install) and `FALSE` upon failure.
#' 
#' @seealso \link{addPackage}
#' 
#' @export
setGeneric("addPackageFile", function(filename, session = NULL) standardGeneric("addPackageFile"))

#' @rdname addPackageFile
setMethod("addPackageFile", signature(session = "character"), function(filename, session) {
  .Deprecated("addPackage")
  stop()
})

#' @rdname addPackageFile
setMethod("addPackageFile", signature(session = "missingOrNULL"), function(filename, session) {
  .Deprecated("addPackage")
  stop()
})

#' @rdname addPackageFile
setMethod("addPackageFile", signature(session = "Session"), function(filename, session) {
  .Deprecated("addPackage")
  stop()
})
