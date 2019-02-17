# Copyright (c) 2019 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# GPL v.3 License
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

#' @rdname description
setMethod('description', signature(ssimObject="character"), function(ssimObject) {
  return(SyncroSimNotFound(ssimObject))
})
  
#' @rdname description
setMethod('description', signature(ssimObject="SsimObject"), function(ssimObject) {
  #ssimObject=myLibrary
  if(class(ssimObject)=="SsimLibrary"){
    desc = command(list(list=NULL,description=NULL,lib=.filepath(ssimObject)),session=.session(ssimObject))
  }
  if(class(ssimObject)=="Project"){
    desc = command(list(list=NULL,description=NULL,lib=.filepath(ssimObject),pid=.projectId(ssimObject)),session=.session(ssimObject))
  }
  if(class(ssimObject)=="Scenario"){
    desc = command(list(list=NULL,description=NULL,lib=.filepath(ssimObject),sid=.scenarioId(ssimObject)),session=.session(ssimObject))
  }
  
  while(max(grepl("  ",desc,fixed=T))){
    desc = gsub("  "," ",desc,fixed=T)
  }
  desc=gsub(". ",".",desc,fixed=T)
  
  desc = desc[2:length(desc)]

  return(desc)
})

#' @rdname description-set
setReplaceMethod(
  f='description',
  signature="character",
  definition=function(ssimObject,value){ 
    return(ssimObject)
})
    
#' @rdname description-set
setReplaceMethod(
  f='description',
  signature="SsimObject",
  definition=function(ssimObject,value){
    inValue = value
    if(length(inValue)>1){
      value=""
      for(i in 1:length(inValue)){
        value=paste0(value,inValue[[i]],sep="\n")
      }
    }
    value = gsub("\n","\\n",value,fixed=T)
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
