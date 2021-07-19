# Copyright (c) 2021 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# GPL v.3 License
#' @include AAAClassDefinitions.R
NULL

#' Installed base packages
#'
#' This function is deprecated.  See \code{\link{package}}.
#'
#' @param ssimObject An object of class \code{\link{Session}} or \code{\link{SsimLibrary}}.
#' 
#' @return 
#' A dataframe of base packages (for Session) or named vector of character strings (for SsimLibrary).
#' 
#' @export
setGeneric("basePackage", function(ssimObject = NULL) standardGeneric("basePackage"))
#' @rdname basePackage

setMethod("basePackage", signature(ssimObject = "character"), function(ssimObject) {
  return(SyncroSimNotFound(ssimObject))
})

#' @rdname basePackage
setMethod("basePackage", signature(ssimObject = "missingOrNULL"), function(ssimObject) {
  .Deprecated("package")
  stop()
})

#' @rdname basePackage
setMethod("basePackage", signature(ssimObject = "Session"), function(ssimObject) {
  .Deprecated("package")
  stop()
})

#' @rdname basePackage
setMethod("basePackage", signature(ssimObject = "SsimLibrary"), function(ssimObject) {
  .Deprecated("package")
  stop()
})
