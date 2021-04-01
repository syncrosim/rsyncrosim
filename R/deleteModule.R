# Copyright (c) 2019 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# GPL v.3 License
#' @include AAAClassDefinitions.R
NULL

#' Delete module or modules
#'
#' This function is now deprecated. See: \code{\link{deletePackage}}.
#'
#' @param name Character string or vector of these. A module or vector of modules 
#'     to remove. See modules() for options.
#' @param session \code{\link{Session}}.
#' @param force logical. If TRUE, delete without requiring confirmation from user.
#' 
#' @return 
#' Returns "saved" if successful, otherwise an error message.
#' 
#' @seealso 
#' \code{\link{deletePackage}}
#' 
#' @export
setGeneric("deleteModule", function(name, session = NULL, force = FALSE) standardGeneric("deleteModule"))

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
