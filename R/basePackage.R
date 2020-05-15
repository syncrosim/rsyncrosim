# Copyright (c) 2019 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# GPL v.3 License
#' @include AAAClassDefinitions.R
NULL

#' Installed base packages
#'
#' Base packages installed with this version of SyncroSim
#'
#' @param ssimObject Session or SsimLibrary.
#' 
#' @return 
#' A dataframe of base packages (for Session) or named vector of character strings (for SsimLibrary)
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
  if ((class(ssimObject) == "character") && (ssimObject == SyncroSimNotFound(warn = F))) {
    return(SyncroSimNotFound())
  }
  tt <- command(c("list", "basepkgs", "csv"), ssimObject)
  out <- .dataframeFromSSim(tt, localNames = T)
  return(out)
})

#' @rdname basePackage
setMethod("basePackage", signature(ssimObject = "Session"), function(ssimObject) {
  tt <- command(c("list", "basepkgs", "csv"), ssimObject)
  out <- .dataframeFromSSim(tt, localNames = T)
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
