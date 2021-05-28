# Copyright (c) 2019 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# GPL v.3 License
#' @include AAAClassDefinitions.R
NULL

#' Installed base packages
#'
#' This retrieves the Base packages installed with this version of SyncroSim.
#' The list of SyncroSim packages can be found \href{https://syncrosim.com/packages/}{here}.
#'
#' @param ssimObject An object of class \code{\link{Session}} or \code{\link{SsimLibrary}}.
#' 
#' @return 
#' A dataframe of base packages (for Session) or named vector of character strings (for SsimLibrary).
#' 
#' @examples
#' \donttest{
#' temp_dir <- tempdir()
#' mySession <- session()
#' myLibrary <- ssimLibrary(name = file.path(temp_dir,"testlib"), session = mySession)
#' 
#' basePackage(myLibrary)
#' }
#' 
#' @export
setGeneric("basePackage", function(ssimObject = NULL) standardGeneric("basePackage"))
#' @rdname basePackage

setMethod("basePackage", signature(ssimObject = "character"), function(ssimObject) {
  return(SyncroSimNotFound(ssimObject))
})

#' @rdname basePackage
setMethod("basePackage", signature(ssimObject = "missingOrNULL"), function(ssimObject) {
  ssimObject <- session()
  if ((class(ssimObject) == "character") && (ssimObject == SyncroSimNotFound(warn = FALSE))) {
    return(SyncroSimNotFound())
  }
  tt <- command(c("list", "basepkgs", "csv"), ssimObject)
  out <- .dataframeFromSSim(tt, localNames = TRUE)
  return(out)
})

#' @rdname basePackage
setMethod("basePackage", signature(ssimObject = "Session"), function(ssimObject) {
  tt <- command(c("list", "basepkgs", "csv"), ssimObject)
  out <- .dataframeFromSSim(tt, localNames = TRUE)
  return(out)
})

#' @rdname basePackage
setMethod("basePackage", signature(ssimObject = "SsimLibrary"), function(ssimObject) {
  oInf <- info(ssimObject)
  property <- NULL
  out <- data.frame(name = subset(oInf, property == "Package Name:")$value)
  out$description <- subset(oInf, property == "Package Description:")$value
  out$version <- subset(oInf, property == "Current Package Version:")$value
  return(out)
})
