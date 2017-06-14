# Copyright © 2017 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License

#' The last date a SsimLibrary/Project/Scenario was modified.
#'
#' The most recent modification date of an SsimLibrary/Project/Scenario
#'
#' @param ssimObject SsimLibrary/Project/Scenario.
#' @export
setGeneric('dateModified',function(ssimObject) standardGeneric('dateModified'))

setMethod('dateModified', signature(ssimObject="SsimLibrary"), function(ssimObject) {
  #ssimObject=myLibrary
  cInfo = info(ssimObject)
  return(subset(cInfo,property=="Last Modified:")$value)
})

setMethod('dateModified', signature(ssimObject="Project"), function(ssimObject) {
  #ssimObject=myProject
  scnInfo = project(ssimObject,summary=T)
  return(scnInfo$lastModified)
})

setMethod('dateModified', signature(ssimObject="Scenario"), function(ssimObject) {
  #ssimObject=newScenario
  scnInfo = scenario(ssimObject,summary=T)
  return(scnInfo$lastModified)
})