# Copyright © 2017 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Description of an SsimLibrary/Project/Scenario.
#'
#' The description of an SsimLibrary/ProjectScenario.
#'
#' @param ssimObject SsimLibrary/Project/Scenario.
#' @export
setGeneric('description',function(ssimObject) standardGeneric('description'))

#' Set the description of an SsimLibrary/Project/Scenario.
#'
#' Set the description of an SsimLibrary/ProjectScenario.
#'
#' @param ssimObject Scenario/Project/SsimLibrary
#' @param value The new description.
#' @export
setGeneric('description<-',function(ssimObject,value) standardGeneric('description<-'))

setMethod('description', signature(ssimObject="SsimLibrary"), function(ssimObject) {
  #ssimObject=myLibrary
  desc = command(list(list=NULL,description=NULL,lib=.filepath(ssimObject)),session=.session(ssimObject))
  return(desc)
})

setMethod('description', signature(ssimObject="Project"), function(ssimObject) {
  #ssimObject=myProject
  desc = command(list(list=NULL,description=NULL,lib=.filepath(ssimObject),pid=.projectId(ssimObject)),session=.session(ssimObject))
  return(desc)
})

setMethod('description', signature(ssimObject="Scenario"), function(ssimObject) {
  #ssimObject=myProject
  desc = command(list(list=NULL,description=NULL,lib=.filepath(ssimObject),sid=.scenarioId(ssimObject)),session=.session(ssimObject))
  return(desc)
})

setReplaceMethod(
  f='description',
  signature="SsimLibrary",
  definition=function(ssimObject,value){
    #x=myScenario;value="New description"
    inValue = value
    if(length(inValue)>1){
      value=""
      for(i in 1:length(inValue)){
        value=paste0(value,inValue[[i]],sep="\n")
      }
    }
    value = gsub("\n","\\n",value,fixed=T)
    #value=paste0('\"',value,'\"')
    args = list(setprop=NULL,lib=.filepath(ssimObject),description=value)
    if(class(ssimObject)=="Project"){args$pid = .projectId(ssimObject)}
    if(class(ssimObject)=="Scenario"){args$sid = .scenarioId(ssimObject)}
    tt = command(args,.session(ssimObject))
    if(!identical(tt,"saved")){
      stop(tt)
    }
    return (ssimObject)
  }
)
