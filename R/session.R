# Author: Josie Hughes
# Date : October 2016
# Version 0.1
# Licence GPL v3
#' @include generics.R
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
      path="c:/gitprojects/syncrosim/_deploy_/current"
      
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
  #TO DO: use 'version' function here once it is working. Update version requirements.
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
setMethod('session', signature(x="SsimLibrary"), function(x,silent,printCmd,defaultModel) x@session)

#' Set a SyncroSim session.
#'
#' Set the Session of a SsimLibrary, Project or Scenario object.
#'
#' @param x An SsimObject.
#' @param value A SyncroSim Session.
#' @return An SyncroSim object containing a Session.
#' @examples
#' myLibrary = ssimLibrary()
#' session(myLibrary)=session()
#' session(myLibrary)
#' @export
setGeneric('session<-',function(x,value) standardGeneric('session<-'))
setReplaceMethod(
  f='session',
  signature="SsimLibrary",
  definition=function(x,value){
    if(class(value)!="Session"){
      stop('Must assign a Session object.')
    }
    x@session = value
    return (x)
  }
)

# @describeIn filepath Path to the Syncrosim console in a Session.
setMethod('filepath', signature(x="Session"), function(x) x@filepath)

#' Check if a Session is silent
#'
#' Checks whether a SyncroSim Session is silent or not.
#'
#' @param session Session or character. A SyncroSim \code{\link{Session}} object or path to a session. If NULL, the default session will be used.
#' @export
setGeneric('silent',function(session) standardGeneric('silent'))
setMethod('silent', signature(session="Session"), function(session) session@silent)
setMethod('silent', signature(session="missingOrNULLOrChar"), function(session) {
  if(class(session)=="character"){
    session = .session(session)
  }else{
    session=.session()
  }
  return(silent(session))
})

#' Set silent property of a Session
#'
#' Set silent property of a sessio to TRUE or FALSE
#'
#' @param session Session
#' @param value logical
#' @export
setGeneric('silent<-',function(session,value) standardGeneric('silent<-'))
setReplaceMethod(
  f='silent',
  signature="Session",
  definition=function(session,value){
    session@silent=value
    return (session)
  }
)

#' Get the default model from a \code{\link{Session}}.
#'
#' Get the default model from a \code{\link{Session}}.
#'
#' @param session Session or character. A SyncroSim \code{\link{Session}} object or path to a session. If NULL, the default session will be used.
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
#' @param session Session
#' @param value character
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

#' Get printCmd a \code{\link{Session}}.
#'
#' Get printCmd a \code{\link{Session}}.
#'
#' @param session Session or character. A SyncroSim \code{\link{Session}} object or path to a session. If NULL, the default session will be used.
#' @export
setGeneric('printCmd',function(session=NULL) standardGeneric('printCmd'))
setMethod('printCmd', signature(session="Session"), function(session) session@printCmd)
setMethod('printCmd', signature(session="missingOrNULLOrChar"), function(session) {
  if(class(session)=="character"){
    session = .session(session)
  }else{
    session=.session()
  }
  return(printCmd(session))
})


#' Installed models
#'
#' Models installed with this version of SyncroSim
#'
#' @param ssimObject Session or SsimLibrary.
#' @export
setGeneric('model',function(ssimObject=NULL) standardGeneric('model'))
setMethod('model', signature(ssimObject="missingOrNULL"), function(ssimObject) {
  ssimOject=session()
  tt=command(c("list","models","csv"),ssimObject)
  out=.dataframeFromSSim(tt,localNames=T)
  return(out)
})
setMethod('model', signature(ssimObject="Session"), function(ssimObject) {
  #x=session()
  tt=command(c("list","models","csv"),ssimObject)
  out=.dataframeFromSSim(tt,localNames=T)
  return(out)
})
setMethod('model', signature(ssimObject="SsimLibrary"), function(ssimObject) {
  #ssimObject=myLib
  oInf = info(ssimObject)
  
  out=list(name=subset(oInf,property=="Model Name:")$value)
  out$description = subset(oInf,property=="Model Description:")$value
  out$version = subset(oInf,property=="Model Current Version:")$value
  out$minVersion = subset(oInf,property=="Model Minimum Version:")$value
  out=unlist(out,use.names=T)
  return(out)
})

#' The SyncroSim version
#'
#' The version of a SyncroSim \code{\link{Session}}.
#'
#' @param x A SyncroSim \code{\link{Session}} object.
#' @export
setGeneric('version',function(x) standardGeneric('version'))
setMethod('version', signature(x="Session"), function(x) {return(command(list(version=NULL),x))})

#' Installed modules
#'
#' Modules installed with this version of SyncroSim
#'
#' @param x A SyncroSim \code{\link{Session}} object.
#' @export
setGeneric('modules',function(x) standardGeneric('modules'))
setMethod('modules', signature(x="Session"), function(x) {
  #x=mySsim
  tt = command(c("listmodules"),x,program="SyncroSim.ModuleManager.exe")
  out = .dataframeFromSSim(tt,colNames=c("name","displayName","version"),csv=F)
  return(out)
})

#' Add modules
#'
#' Add module or modules to SyncroSim
#'
#' @param filename The path to an .ssimpkg file on disk, or a vector of filepaths
#' @param session A SyncroSim \code{\link{Session}} object.
#' @export
setGeneric('addModule',function(filename,session=NULL) standardGeneric('addModule'))
setMethod('addModule', signature(filename="character"), function(filename,session) {
    #x=mySsim
    #value=c("C:/Program Files/SyncroSim/1/CorePackages/stockflow.ssimpkg","C:/Program Files/SyncroSim/1/CorePackages/dynmult.ssimpkg")
    #value="C:/Program Files/SyncroSim/1/CorePackages/stockflow.ssimpkg"
  
  if(is.null(session)){session=.session()}
    for(i in seq(length.out=length(filename))){
      #i=1
      cVal = filename[i]
      if(!file.exists(cVal)){
        stop(paste0("Cannot find ",cVal,"."))
      }
      tt = command(args=list(queue=cVal),session,program="SyncroSim.ModuleManager.exe")
    }
    tt = command(args=list(installqueue=NULL),session,program="SyncroSim.ModuleManager.exe")
    #session@datasheetNames = .datasheets(x,scope="all",refresh=T)
    return (tt)
  }
)

#' Delete module or modules
#'
#' Delete module or modules from this version of SyncroSim.
#' Note that removing a module can be difficult to undo.
#' To restore the module the user will need to provide a .ssimpkg file or reinstall SyncroSim.
#' Thus, \code{deleteModule} requires confirmation from the user.
#'
#' @param name A module or vector of modules to remove. \code{\link{modules()}} for options.
#' @param session A SyncroSim \code{\link{Session}} object.
#' @export
setGeneric('deleteModule',function(name,session=NULL) standardGeneric('deleteModule'))
setMethod('deleteModule', signature(session="missingOrNULLOrChar"), function(name,session) {
  session=.session(session)
  return(deleteModule(name,session))
})
setMethod('deleteModule', signature(session="Session"), function(name,session) {
    #name = "sample-basic-dotnet";session=session()
    installedModules=modules(session)
    retList = list()
    for(i in seq(length.out=length(name))){
      #i = 1
      cVal = name[i]
      if(!is.element(cVal,installedModules$name)){
        print(paste0("Module ",cVal," is not installed, so cannot be removed."))
        next
      }

      answer <- readline(prompt=paste0("To restore ",cVal," after removing it you will need to provide a .ssimpkg file or reinstall SyncroSim.\nDo you really want to remove the module? (y/n): "))
      if(answer=="y"){
        tt = command(args=list(removemodule=cVal),session,program="SyncroSim.ModuleManager.exe")

        installedModules = modules(session)
        if(is.element(cVal,installedModules$name)){
          stop(paste0('Error: failed to remove module ',cVal))
        }
        retList[[cVal]]=tt
      }else{
        retList[[cVal]]="skipped"
      }
    }
    return (retList)
  })



