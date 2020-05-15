# Copyright (c) 2019 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# GPL v.3 License
#' @include AAAClassDefinitions.R
NULL

#' Installed models
#'
#' Deprecated.  See: \code{\link{package}}
#'
#' @param ssimObject Session or SsimLibrary.
#' 
#' @return 
#' A dataframe of models (for Session) or named vector of character strings (for SsimLibrary)
#' 
#' @export
setGeneric("model", function(ssimObject = NULL) standardGeneric("model"))

#' @rdname model
setMethod("model", signature(ssimObject = "character"), function(ssimObject) {
  return(SyncroSimNotFound(ssimObject))
})

#' @rdname model
setMethod("model", signature(ssimObject = "missingOrNULL"), function(ssimObject) {
  ssimObject <- session()
  if ((class(ssimObject) == "character") && (ssimObject == SyncroSimNotFound(warn = FALSE))) {
    return(SyncroSimNotFound())
  }
  .Deprecated("basePackage")
  stop()
})

#' @rdname model
setMethod("model", signature(ssimObject = "Session"), function(ssimObject) {
  .Deprecated("basePackage")
  stop()
})

#' @rdname model
setMethod("model", signature(ssimObject = "SsimLibrary"), function(ssimObject) {
  .Deprecated("basePackage")
  stop()
})
