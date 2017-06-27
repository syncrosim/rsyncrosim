# Copyright (c) 2017 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' The owner of a SsimLibrary/Project/Scenario.
#'
#' The owner of an SsimLibrary/ProjectScenario
#'
#' @param ssimObject SsimLibrary/Project/Scenario.
#' @export
setGeneric('owner',function(ssimObject) standardGeneric('owner'))

#' Set the owner of an SsimLibrary/Project/Scenario.
#'
#' Set the owner of an SsimLibrary/Project/Scenario.
#'
#' @param ssimObject Scenario/Project/SsimLibrary
#' @param value The new owner.
#' @export
setGeneric('owner<-',function(ssimObject,value) standardGeneric('owner<-'))
#' @rdname owner
setMethod('owner', signature(ssimObject="SsimLibrary"), function(ssimObject) {
  #ssimObject=myLibrary
  cInfo = info(ssimObject)
  property=NULL
  return(subset(cInfo,property=="Owner:")$value)
})
#' @rdname owner
setMethod('owner', signature(ssimObject="Project"), function(ssimObject) {
  #ssimObject=myProject
  scnInfo = project(ssimObject,summary=T)
  return(scnInfo$owner)
})
#' @rdname owner
setMethod('owner', signature(ssimObject="Scenario"), function(ssimObject) {
  #ssimObject=newScenario
  scnInfo = scenario(ssimObject,summary=T)
  return(scnInfo$owner)
})

#' @rdname owner-set
setReplaceMethod(
  f='owner',
  signature="SsimObject",
  definition=function(ssimObject,value){
    #x=myScenario;value="New description"
    args = list(setprop=NULL,lib=.filepath(ssimObject),owner=value)
    if(class(ssimObject)=="Project"){args$pid = .projectId(ssimObject)}
    if(class(ssimObject)=="Scenario"){args$sid = .scenarioId(ssimObject)}
    tt = command(args,.session(ssimObject))
    if(!identical(tt,"saved")){
      stop(tt)
    }
    return (ssimObject)
  }
)
