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
      envVars = Sys.getenv(c("PROGRAMFILES","PROGRAMFILES(X86)"),names=F)
      envVars=envVars[envVars!=""]
      for(i in seq(length.out=length(envVars))){
        #i=1
        cPath = paste0(envVars[i],"\\SyncroSim\\1")
        if(file.exists(paste0(cPath,"\\SyncroSim.Console.exe"))){path=cPath;break}
      }
    }
  }
  if(is.null(path)){
    stop('Default SyncroSim installation not found. Either install SyncroSim in the default location, or explicitly set the session path. See ?session for details.')
  }

  .Object@filepath=gsub("\\","/",gsub("/SyncroSim.Console.exe","",path,fixed=T),fixed=T)
  .Object@silent=silent
  .Object@printCmd=printCmd
  
  #check default model is valid
  modelOptions = models(.Object)
  model=gsub(":model-transformer","",defaultModel,fixed=T)
  if(!is.element(model,modelOptions$shortName)){
    stop(paste("Model type",defaultModel,"not recognized. Options are:",paste0(modelOptions$shortName,collapse=",")))
  }
  
  .Object@defaultModel=model

  vs = command(list(version=NULL),.Object)
  if(!grepl("Core Assembly Version",vs[[2]])){stop(vs)}
  
  #TO DO: use 'version' function here once it is working. Update version requirements.
  vs = gsub("Core Assembly Version: ","",vs[[2]],fixed=T)
  vs = as.numeric(gsub(".","",vs,fixed=T))
  if(vs<10430){#1.0.43.0
    stop("rsyncrosim requires at least SyncroSim version 1.0.43.0.")
  }
  #else{
    #check for development versions that do not have all required functionality
  #  checkCmd =command(list(export=NULL,datasheet=NULL,help=NULL),.Object)
  #  if(max(grepl("allsheets",checkCmd))==0){
  #    stop("rsyncrosim requires a more recent version of SyncroSim.")
  #  }
  #}

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
setGeneric('session',function(x=NULL,silent=F,printCmd=F,defaultModel="stsim") standardGeneric('session'))

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
    modelOptions = models(session)
    model=gsub(":model-transformer","",value,fixed=T)
    if(!is.element(model,modelOptions$shortName)){
      stop(paste("Model type",value,"not recognized. Options are:",paste0(modelOptions$shortName,collapse=",")))
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
#' @param x A SyncroSim \code{\link{Session}} object.
#' @export
setGeneric('models',function(x) standardGeneric('models'))
setMethod('models', signature(x="Session"), function(x) {
  #x=session()
  tt=command(c("list","models","csv"),x)
  out=.dataframeFromSSim(tt,localNames=T)
  out$shortName = gsub(":model-transformer","",out$name,fixed=T)
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

#' Remove modules
#'
#' Remove module or modules to this version of SyncroSim.
#' Note that removing a module can be difficult to undo.
#' To restore the module the user will need to provide a .ssimpkg file or reinstall SyncroSim.
#' Thus, \code{removeModules} requires confirmation from the user.
#'
#' @param x A SyncroSim \code{\link{Session}} object.
#' @param value A module or vector of modules to remove. \code{\link{modules()}} for options.
#' @export
setGeneric('removeModules<-',function(x,value) standardGeneric('removeModules<-'))
setReplaceMethod(
  f='removeModules',
  signature="Session",
  definition=function(x,value){
    #value = "stsim-stock-flow";x=mySsim
    installedModules=modules(x)
    for(i in seq(length.out=length(value))){
      #i = 1
      cVal = value[i]
      if(!is.element(cVal,installedModules$name)){
        print(paste0("Module ",cVal," is not installed, so cannot be removed."))
        next
      }

      answer <- readline(prompt=paste0("To restore ",cVal," after removing it you will need to provide a .ssimpkg file or reinstall SyncroSim.\nDo you really want to remove the module? (y/n): "))
      if(answer=="y"){
        tt = command(args=list(removemodule=cVal),x,program="SyncroSim.ModuleManager.exe")

        installedModules = modules(x)
        if(is.element(cVal,installedModules$name)){
          stop(paste0('Error: failed to remove module ',cVal))
        }
      }
    }
    return (x)
  }
)



