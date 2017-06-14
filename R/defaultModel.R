# Copyright © 2017 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License

#' Get the default model from a Session.
#'
#' Get the default model from a Session.
#'
#' @param session Session or character. A Session object or path to a session. If NULL, the default session will be used.
#' @return The default model of a Session.
#' @export
setGeneric('defaultModel',function(session=NULL) standardGeneric('defaultModel'))
setMethod('defaultModel', signature(session="Session"), function(session) session@defaultModel)
setMethod('defaultModel', signature(session="NULL"), function(session) {
  if(class(session)=="character"){
    session = .session(session)
  }else{
    session=.session()
  }
  return(defaultModel(session))
})

#' Set defaultModel of a Session
#'
#' Set defaultModel of a session
#'
#' @param session Session.
#' @param value character. A SyncroSim model. See model() for options.
#' @export
setGeneric('defaultModel<-',function(session,value) standardGeneric('defaultModel<-'))
setReplaceMethod(
  f='defaultModel',
  signature="Session",
  definition=function(session,value){
    #check default model is valid
    modelOptions = model(session)
    model=gsub(":model-transformer","",value,fixed=T)
    if(!is.element(model,modelOptions$name)){
      stop(paste("Model type",value,"not recognized. Options are:",paste0(modelOptions$name,collapse=",")))
    }
    session@defaultModel=value
    return (session)
  }
)
