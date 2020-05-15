# Copyright (c) 2019 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# GPL v.3 License
#' @include AAAClassDefinitions.R
NULL

#' Delete module or modules
#'
#' Deprecated.  See: \code{\link{deletePackage}}
#'
#' @param name Character string or vector of these. A module or vector of modules to remove. See modules() for options.
#' @param session Session.
#' @param force logical. If T, delete without requiring confirmation from user.
#' 
#' @return "saved" or error message.
#' 
#' @export
setGeneric("deleteModule", function(name, session = NULL, force = F) standardGeneric("deleteModule"))

#' @rdname deleteModule
setMethod("deleteModule", signature(session = "missingOrNULLOrChar"), function(name, session, force) {
  .Deprecated("deletePackage")
  stop()
})

#' @rdname deleteModule
setMethod("deleteModule", signature(session = "Session"), function(name, session, force) {
  .Deprecated("deletePackage")
  stop()
})
