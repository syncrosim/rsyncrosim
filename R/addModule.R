# Copyright (c) 2021 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# GPL v.3 License
#' @include AAAClassDefinitions.R
NULL

#' Add module
#'
#' This function is deprecated.  See \code{\link{addPackage}}.
#'
#' @param filename Character string or vector of these. The path to an .ssimpkg file on disk, or a vector of filepaths.
#' @param session Session.
#' 
setGeneric("addModule", function(filename, session = NULL) standardGeneric("addModule"))

#' @rdname addModule
setMethod("addModule", signature(filename = "character"), function(filename, session) {
  .Deprecated("addPackage")
  stop()
})
