# Author: Josie Hughes
# Date : October 2016
# Version 0.1
# Licence GPL v3
#' @include generics.R
NULL
#' SyncroSim Session class
#'
#' A SyncroSim Session object contains a link to SyncroSim.
#' \code{SSimLibrary}, \code{Project} and \code{Scenario} objects contain a \code{Session} used to query and modify the object.
#'
#' @examples
#' # Create or load a library using a non-default Session
#' mySession = session("C:/Program Files/SyncroSim/1/SyncroSim.Console.exe")
#' myLib = ssimLibrary(name="stsim",model="st-sim",session=mySession)
#' session(myLib)
#'
#' showMethods(class="Session",where=loadNamespace("rsyncrosim")) #Methods for the Session
#' filepath(mySession)   # Lists the folder location of syncrosim session
#' version(mySession)   # Lists the version of syncrosim session
#' modules(mySession)   # Dataframe of the modules installed with this version of syncrosim.
#' models(mySsim) # Dataframe of the models installed with this version of syncrosim.
#'
#' # Add and remove modules
#' removeModules(mySsim) = "stsim-stock-flow"
#' is.element("stsim-stock-flow",modules(mySsim)$shortName)
#' addModules(mySsim) = "C:/Program Files/SyncroSim/1/CorePackages/stockflow.ssimpkg"
#' addModules(mySsim) = c("C:/Program Files/SyncroSim/1/CorePackages/stockflow.ssimpkg","C:/Program Files/SyncroSim/1/CorePackages/dynmult.ssimpkg")
#' is.element("stsim-stock-flow",modules(mySsim)$shortName)
#'
#' # Create or load a library using a default Session
#' myLib = ssimLibrary(name="stsim",model="stsim")
#' session(myLib)
#' @slot filepath The path to SyncroSim
#' @slot silent If TRUE (default), warnings from the console are ignored. Otherwise they are printed.
#' @name Session-class
#' @rdname Session-class
#' @export Session
Session <- setClass("Session", representation(filepath="character",silent="logical"))
# @name Session
# @rdname Session-class
setMethod(f='initialize',signature="Session",definition=function(.Object,path,silent=T){
  #path = NULL;silent=F;.Object=ssimSession
  #Check validity of console filepath.
  if(!is.null(path)){
    if(!grepl("SyncroSim.Console.exe",path,fixed=T)){
      path=paste0(path,"/SyncroSim.Console.exe")
    }

    if(!file.exists(path)){
      stop(paste("SyncroSim console could not be found at:",path))
    }
  }else{
    #try which
    whichPath = Sys.which("SyncroSim.Console.exe")
    if (file.exists(whichPath[1])){consolePath=whichPath[1]}
    if(is.null(path)){
      #TO DO: what is best way to find console on all systems
      #Default installation locations?
      consolePathPossibilities = c("C:/Users/Josie Hughes/SyncroSim/syncrosim-windows-1-0-38-x64 (bug)","C:/svnprojects/SyncroSim-1/WinForm/bin/x86/Debug","C:/Users/Josie Hughes/SyncroSim/syncrosim-windows-1-0-38-x64","C:/Program Files/SyncroSim/1","C:/Program Files/SyncroSim/1")
      for(i in seq(length.out=length(consolePathPossibilities))){
        if(file.exists(consolePathPossibilities[i])){path=consolePathPossibilities[i];break}
      }
    }
  }
  if(is.null(path)){
    stop('SyncroSim.Console.exe not found. Please set consolePath to the location of the SyncroSim console.')
  }

  .Object@filepath=gsub("/SyncroSim.Console.exe","",path,fixed=T)
  .Object@silent=silent

  vs = command(list(version=NULL),.Object)
  vs = gsub("Core Assembly Version: ","",vs[[2]],fixed=T)
  vs = as.numeric(gsub(".","",vs,fixed=T))
  if(vs<10380){
    stop("rsyncrosim requires at least SyncroSim version 1.0.38.0.")
  }else{
    #check for development versions that do not have all required functionality
    checkCmd =command(list(export=NULL,datasheet=NULL,help=NULL),.Object)
    if(max(grepl("allsheets",checkCmd))==0){
      stop("rsyncrosim requires a more recent version of SyncroSim.")
    }
  }

  return(.Object)
})

# @describeIn session Create a SyncroSim Session from a filepath or get default Session.
setMethod('session', signature(x="missingOrNULLOrChar"), function(x,silent=T) {
  return(new("Session",x,silent))
})

# @describeIn filepath Path to the Syncrosim console in a Session.
setMethod('filepath', signature(x="Session"), function(x) x@filepath)

#' Check if a Session is silent
#'
#' Checks whether a SyncroSim Session is silent or not.
#'
#' @param x A SyncroSim \code{\link{Session}} object.
#' @export
setGeneric('silent',function(x) standardGeneric('silent'))
setMethod('silent', signature(x="Session"), function(x) x@silent)

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
  tt = command(c("listmodules"),x,program="/SyncroSim.ModuleManager.exe")
  out = .dataframeFromSSim(tt,colNames=c("name","displayName","version"),csv=F)
  return(out)
})

#' Add modules
#'
#' Add module or modules to this version of SyncroSim
#'
#' @param x A SyncroSim \code{\link{Session}} object.
#' @param value The path to an .ssimpkg file on disk, or a vector of filepaths
#' @export
setGeneric('addModules<-',function(x,value) standardGeneric('addModules<-'))
setReplaceMethod(
  f='addModules',
  signature="Session",
  definition=function(x,value){
    #x=mySsim
    #value=c("C:/Program Files/SyncroSim/1/CorePackages/stockflow.ssimpkg","C:/Program Files/SyncroSim/1/CorePackages/dynmult.ssimpkg")
    #value="C:/Program Files/SyncroSim/1/CorePackages/stockflow.ssimpkg"
    for(i in seq(length.out=length(value))){
      #i=1
      cVal = value[i]
      if(!file.exists(cVal)){
        stop(paste0("Cannot find ",cVal,"."))
      }
      tt = command(args=list(queue=cVal),x,program="/SyncroSim.ModuleManager.exe")
    }
    tt = command(args=list(installqueue=NULL),x,program="/SyncroSim.ModuleManager.exe")
    x@datasheetNames = .datasheets(x,scope="all",refresh=T)
    return (x)
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
        tt = command(args=list(removemodule=cVal),x,program="/SyncroSim.ModuleManager.exe")

        installedModules = modules(x)
        if(is.element(cVal,installedModules$name)){
          stop(paste0('Error: failed to remove module ',cVal))
        }
      }
    }
    return (x)
  }
)



