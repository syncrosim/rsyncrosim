# Copyright (c) 2017 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# GPL v.3 License
#' @include AAAClassDefinitions.R
NULL

#' Read-only status of an SsimLibrary/Project/Scenario.
#'
#' Whether or not an SsimLibrary/ProjectScenario is read-only.
#'
#' @param ssimObject SsimLibrary/Project/Scenario.
#' @return logical.
#' @export
setGeneric('readOnly',function(ssimObject) standardGeneric('readOnly'))
#' @rdname readOnly
setMethod('readOnly', signature(ssimObject="SsimLibrary"), function(ssimObject) {
  #ssimObject=myLibrary
  cInfo = info(ssimObject)
  property=NULL
  oVal  = subset(cInfo,property=="Read Only:")$value
  rVal=oVal
  if(oVal=="Yes"){rVal=T}
  if(oVal=="No"){rVal=F}
  return(rVal)
})
#' @rdname readOnly
setMethod('readOnly', signature(ssimObject="Project"), function(ssimObject) {
  #ssimObject=myProject
  scnInfo = project(ssimObject,summary=T)
  return(scnInfo$readOnly)
})
#' @rdname readOnly
setMethod('readOnly', signature(ssimObject="Scenario"), function(ssimObject) {
  #ssimObject=newScenario
  scnInfo = scenario(ssimObject,summary=T)
  return(scnInfo$readOnly)
})


#' Set the read/write status of an SsimLibrary/Project/Scenario.
#'
#' Set the read-only status of an SsimLibrary/Project/Scenario. 
#' Applies to child objects if ssimObject is an SsimLibrary or Project.
#'
#' @param ssimObject Scenario/Project/SsimLibrary
#' @param value Logical. If T the ssimObject will be read-only.
#' @export
setGeneric('readOnly<-',function(ssimObject,value) standardGeneric('readOnly<-'))
#' @rdname readOnly-set
setReplaceMethod(
  f='readOnly',
  signature="SsimObject",
  definition=function(ssimObject,value){
    #value = F;ssimObject=myLibrary
    if(class(value)!="logical"){
      stop("readOnly must be TRUE or FALSE.")
    }
    if(value==T){readOnly = "yes"}else{readOnly="no"}
    args = list(setprop=NULL,lib=.filepath(ssimObject),readonly=readOnly)
    if(class(ssimObject)=="Project"){args$pid = .projectId(ssimObject)}
    if(class(ssimObject)=="Scenario"){args$sid = .scenarioId(ssimObject)}
    tt = command(args,.session(ssimObject))
    if(!identical(tt,"saved")){
      stop(tt)
    }
    return (ssimObject)
  }
)

