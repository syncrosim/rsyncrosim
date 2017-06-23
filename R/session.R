# Copyright © 2017 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

# @name Session
# @rdname Session-class
setMethod(f='initialize',signature="Session",definition=function(.Object,path,silent=F,printCmd=F,defaultModel="stsim"){
  #path = NULL;silent=F;.Object=session()
  #Check validity of console filepath.
  if(!is.null(path)){
    if(!grepl("SyncroSim.Console.exe",path,fixed=T)){
      path=paste0(path,"/SyncroSim.Console.exe")
    }

    if(!file.exists(path)){
      stop(paste("SyncroSim console could not be found at:",path))
    }
  }else{
    #look for the regular installed version in the default installation directories
    #There is no relevant info in the registry
    #registry = readRegistry("Applications\\SyncroSim.WinForm.exe\\shell\\open\\command",hive="HCR")
    #TO DO: debug this on linux
    #path=NULL
    if(is.null(path)){
      #TO DO: what is best way to find console on all systems
      #Default installation locations?
      temp = "c:/gitprojects/syncrosim/_deploy_/current"
      if(file.exists(paste0(temp,"/SyncroSim.Console.exe"))){
        path=temp
      }
      
      if(0){
      #TO DO: change to default path once SyncroSim v2 is released.
      envVars = Sys.getenv(c("PROGRAMFILES","PROGRAMFILES(X86)"),names=F)
      envVars=envVars[envVars!=""]
      for(i in seq(length.out=length(envVars))){
        #i=1
        cPath = paste0(envVars[i],"\\SyncroSim\\1")
        if(file.exists(paste0(cPath,"\\SyncroSim.Console.exe"))){path=cPath;break}
      }}
    }
  }
  if(is.null(path)){
    stop('Default SyncroSim installation not found. Either install SyncroSim in the default location, or explicitly set the session path. See ?session for details.')
  }

  .Object@filepath=gsub("\\","/",gsub("/SyncroSim.Console.exe","",path,fixed=T),fixed=T)
  .Object@silent=silent
  .Object@printCmd=printCmd
  
  #check default model is valid
  modelOptions = model(.Object)
  model=gsub(":model-transformer","",defaultModel,fixed=T)
  if(!is.element(model,modelOptions$name)){
    stop(paste("Model type",defaultModel,"not recognized. Options are:",paste0(modelOptions$name,collapse=",")))
  }
  
  .Object@defaultModel=model

  vs = command(list(version=NULL),.Object)
  if((length(vs)>1)){
    if(grepl("Core Assembly Version:",vs[[2]],fixed=T)){stop("SyncroSim version 2.0.0 or greater is required.")}
    stop(vs)
  }

  if(!grepl("Version is:",vs)){
    stop(vs)
  }
  vs = gsub("Version is: ","",vs,fixed=T)
  vs = as.numeric(strsplit(vs,".",fixed=T)[[1]])
  vs = vs[1]*10000+vs[2]+vs[3]/1000#assume no value >1000 in any position
  if(vs<20000){#1.0.43.0
    stop("rsyncrosim requires at least SyncroSim version 2.0.0.")
  }
  return(.Object)
})

#' Start or get a SyncroSim session.
#'
#' Methods to create a Syncrosim session or fetch one from a SsimLibrary, Project or Scenario object.
#' @param x Character or SsimObject. A path to SyncroSim.Console.exe or an object containing a Session. If NULL the installed version of syncrosim in the registry is used.
#' @param silent Logical. Applies only if x is a path or NULL. If TRUE, warnings from the console are ignored. Otherwise they are printed.
#' @param printCmd Logical. Applies only if x is a path or NULL. If TRUE, arguments passed to the SyncroSim console are also printed. Helpful for debugging. FALSE by default.
#' @param defaultModel Character. Applies only if x is a path or NULL. The name of a SyncroSim model type. "stsim" by default.
#' @return An SyncroSim Session object containing a valid console path.
#' @examples
#' # Look for SyncroSim in the usual places
#' mySession = session()
#' path(mySession)
#'
#' # Specify a SyncroSim version
#' mySession = session("C:/Program Files/SyncroSim/1/SyncroSim.Console.exe")
#'
#' # Get the session from an SsimLibrary
#' myLib = ssimLibrary(name="stsim")
#' session(myLib)
#'
#' # Assign a session to a SyncroSim library
#' session(myLib)=session()
#' @export
setGeneric('session',function(x=NULL,silent=T,printCmd=F,defaultModel="stsim") standardGeneric('session'))

#' @describeIn session Create a SyncroSim Session from a filepath or get default Session.
setMethod('session', signature(x="missingOrNULLOrChar"), function(x,silent,printCmd,defaultModel) {
  return(new("Session",x,silent,printCmd,defaultModel))
})

#' @describeIn session Get the Session associated with an SsimObject.
setMethod('session', signature(x="SsimObject"), function(x,silent,printCmd,defaultModel) x@session)

#' Set a SyncroSim session.
#'
#' Set the Session of a SsimLibrary, Project or Scenario object.
#'
#' @details 
#' 
#' In order to avoid problems with SyncroSim version compatibility and library updating, 
#' the new session must have the same filepath as the session of the SsimObject e.g. filepath(value)==filepath(session(ssimObject))
#' 
#' @param ssimObject SsimObject/Project/Scenario.
#' @param value A SyncroSim Session.
#' @return An SyncroSim object containing a Session.
#' @examples
#' myLibrary = ssimLibrary()
#' session(myLibrary)=session()
#' session(myLibrary)
#' @export
setGeneric('session<-',function(ssimObject,value) standardGeneric('session<-'))
setReplaceMethod(
  f='session',
  signature="SsimLibrary",
  definition=function(ssimObject,value){
    if(class(value)!="Session"){
      stop('Must assign a Session object.')
    }
    if(.filepath(value)!=.filepath(.session(ssimObject))){
      stop('The new session must have the same filepath as the session of the SsimObject e.g. filepath(value)==filepath(session(ssimObject))')
    }
    ssimObject@session = value
    return (ssimObject)
  }
)





