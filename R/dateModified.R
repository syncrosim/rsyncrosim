# Copyright (c) 2017 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# GPL v.3 License
#' @include AAAClassDefinitions.R
NULL

#' The last date a SsimLibrary/Project/Scenario was modified.
#'
#' The most recent modification date of an SsimLibrary/Project/Scenario
#'
#' @param ssimObject SsimLibrary/Project/Scenario.
#' @export
setGeneric('dateModified',function(ssimObject) standardGeneric('dateModified'))
#' @rdname dateModified
setMethod('dateModified', signature(ssimObject="character"), function(ssimObject) {
  return(SyncroSimNotFound(ssimObject))})
  
#' @rdname dateModified
setMethod('dateModified', signature(ssimObject="SsimLibrary"), function(ssimObject) {
  cInfo = info(ssimObject)
  property=NULL
  return(subset(cInfo,property=="Last Modified:")$value)
})
#' @rdname dateModified
setMethod('dateModified', signature(ssimObject="Project"), function(ssimObject) {
  scnInfo = project(ssimObject,summary=T)
  return(scnInfo$lastModified)
})
#' @rdname dateModified
setMethod('dateModified', signature(ssimObject="Scenario"), function(ssimObject) {
  scnInfo = scenario(ssimObject,summary=T)
  return(scnInfo$lastModified)
})