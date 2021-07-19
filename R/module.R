# Copyright (c) 2021 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# GPL v.3 License
#' @include AAAClassDefinitions.R
NULL

#' Installed modules
#'
#' Deprecated, modules are now distributed in Packages, see: \code{\link{package}}.
#'
#' @param session \code{\link{Session}}.
#' 
#' @return 
#' Returns a \code{dataframe} of modules.
#' 
#' @export
setGeneric("module", function(session) standardGeneric("module"))

#' @rdname module
setMethod("module", signature(session = "missingOrNULL"), function(session) {
  .Deprecated("package")
  stop()
})

#' @rdname module
setMethod("module", signature(session = "character"), function(session) {
  .Deprecated("package")
  stop()
})

#' @rdname module
setMethod("module", signature(session = "Session"), function(session) {
  .Deprecated("package")
  stop()
})
