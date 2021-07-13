# Copyright (c) 2019 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# GPL v.3 License
#' @include AAAClassDefinitions.R
NULL

#' Deletes a package from your SyncroSim installation.
#' 
#' This function is now deprecated. See: \code{\link{removePackage}}.
#' 
#' @param name Character. The name of the package to delete.
#' @param session An object of class \code{\link{Session}}.
#' @param force Logical. If TRUE, delete without requiring confirmation from 
#'     the user.
#' 
#' @return 
#' This function invisibly returns `TRUE` upon success (i.e.successful 
#' deletion) and `FALSE` upon failure.
#' 
#' @seealso 
#' \code{\link{removePackage}}
#' 
#' @export
setGeneric("deletePackage", function(name, session = NULL, force = FALSE) standardGeneric("deletePackage"))

#' @rdname deletePackage
setMethod("deletePackage", signature(session = "character"), function(name, session, force) {
  .Deprecated("removePackage")
  stop()
})

#' @rdname deletePackage
setMethod("deletePackage", signature(session = "missingOrNULL"), function(name, session, force) {
  .Deprecated("removePackage")
  stop()
})

#' @rdname deletePackage
setMethod("deletePackage", signature(session = "Session"), function(name, session, force) {
  .Deprecated("removePackage")
  stop()
})
