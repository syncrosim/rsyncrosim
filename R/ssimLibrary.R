# Author: Josie Hughes
# Date : October 2016
# Version 0.1
# Licence GPL v3
#' @include generics.R
NULL
#' SyncroSim Library class
#'
#' \code{SSimLibrary} object representing a SyncroSim Library.
#'
#' @seealso See \code{\link{ssimLibrary}} for options when creating or loading an SyncroSim library.
#' @examples
#' # Create or load and query an STSim library.
#' myLib = ssimLibrary(model="st-sim")
#' session(myLib)
#' filepath(myLib)
#' info(myLib)
#' @slot session The SyncroSim session.
#' @slot filepath The path to the library on disk.
#' @name SSimLibrary-class
#' @rdname SSimLibrary-class
#' @export SSimLibrary
SSimLibrary <- setClass("SSimLibrary", representation(session="Session",filepath="character"))
# @name SSimLibrary
# @rdname SSimLibrary-class
setMethod(f="initialize",signature="SSimLibrary",
    definition=function(.Object,model=NULL,name=NULL,aSession=NULL,backup=F,backupName="backup",backupOverwrite=T){
    #model="stsim";name="stsim";aSession=mySsim
    #if a syncrosim session is not provided, make one
    if(is.null(aSession)){
      aSession = session()
    }

    modelOptions = models(aSession)

    #modelOptions=list("st-sim"=list(modelCmd="stsim:model-transformer",modelName="ST-Sim State and Transition"))
    if(!is.null(model)){
      model=gsub(":model-transformer","",model,fixed=T)
      if(!is.element(model,modelOptions$name)){
        stop(paste("Model type",model,"not recognized. Options are:",paste0(modelOptions$name,collapse=",")))
      }
    }

    #If no name is provided, search for a library in the current working directory
    #If there is one library, set name to that library.
    #If there is more than one library, complain.
    #If there are no libraries, set name to model - if model is NULL complain.
    if(is.null(name)){
      fList = list.files(pattern="\\.ssim")
      if(length(fList)>1){
        stop(paste0("The working directory contains more than one SyncroSimLibrary - specify a name:",paste(fList,collapse=",")))
      }
      if(length(fList)==1){
        name = fList[1]
      }
      if(length(fList)==0){
        if(is.null(model)){
          if(nrow(modelOptions)==1){
            model = modelOptions$name
          }else{
            stop(paste0("Please specify a model type for a new library. Options are:",paste(modelOptions$name,collapse=",")))
          }
        }
        name=model
      }
    }

    path <- .fullFilename(name)
    if(!grepl(".ssim",path)) path=paste0(path,".ssim")

    #if library does not exist on disk, create it
    if(!file.exists(path)){
      if(is.null(model)){
        stop('Specify a model for the new library.')
      }
      pathBits = strsplit(path,"/")[[1]]
      dir.create(paste(head(pathBits,-1),collapse="/"),showWarnings=F)

      args = list(create=NULL,library=NULL,name=path,model=modelOptions$command[modelOptions$name==model])
      cStatus = command(args,aSession)
      #cStatus=command(args,aSession)
    }else{
      #x="C:/Temp/NewLibrary.ssim"
      if(backup){
        backupName = gsub(".ssim",paste0("_",backupName,".ssim"),path,fixed=T)
        if(file.exists(backupName)&!backupOverwrite){
          stop(paste0('Backup ',backupName,' already exists. Set backupOverwrite=T or provide a different backupName.'))
        }
        file.copy(path, backupName, overwrite=T)
      }
    }
    #ensure the primaryModule specified matches the primaryModule on disk
    args = list(list=NULL,library=NULL,lib=path)
    cStatus = command(args,aSession)
    #cStatus= command(args,aSession)
    if(!is.null(model)){
      expectedModule = modelOptions$description[modelOptions$name==model]
      if(!grepl(expectedModule,cStatus[2])){
        stop(paste0("A library of that name and a different model type ",cStatus[2]," already exists."))
      }
    }
    .Object@session=aSession
    .Object@filepath=path
    return(.Object)
  }
)
#' Create or open a library.
#'
#' Creates or opens an \code{\link{SSimLibrary}} object representing a SyncroSim library.
#'
#' @details
#' \itemize{
#'   \item {If given no name and no model: }{Opens an existing SyncroSim library in
#'   the current working directory - returns an error if more than one library exists. If library does not exist and only one model is installed - creates a library of that type.}
#'   \item {If given a model but no name: }{Opens or creates a library called <model>.ssim in the current working directory.}
#'   \item {If given a name but no model: }{Attempts to open a library of that name. Returns an error if that library does not already exist.}
#'   \item {If given a name and a model: }{Opens or creates a library called <name>.ssim. Returns an error if the library already exists but is a different type of model.}
#' }
#' @param model The model type. Optional when loading an existing library.
#' @param name A library file name or library file path. If not a path library is created or opened in the current working directory.
#' @param aSession A SyncroSim \code{Session}. If NULL, the default SyncroSim Session will be used.
#' @param backup If TRUE, a backup copy is made when an existing library is opened.
#' @param backupName Added to a library filepath to create a backup library.
#' @param backupOverwrite If TRUE, the existing backup of a library (if any) will be overwritten.
#' @return An \code{SSimLibrary} object representing a SyncroSim library.
#' @examples
#' # See the installed models
#' models(session())
#'
#' # Create a library called <model>.ssim in the current working directory.
#' myLib = ssimLibrary(model="stsim")
#' session(myLib) #The SycroSim session
#' filepath(myLib) #Path to the file on disk.
#' info(myLib) #Model type and other library information.
#'
#' # Open an existing SyncroSim library in the current working directory - don't make a backup copy.
#' myLib = ssimLibrary()
#'
#' # Create a library with a name in the current working directory
#' myLib2 = ssimLibrary(name="Lib2",model="stsim")
#'
#' # Create a library with a name in another directory
#' myLib3 = ssimLibrary(name=paste0(getwd(),"/Temp/Lib3"),model="stsim")
#'
#' # Create or load a library using a specific session
#' mySession = session("C:/Program Files/SyncroSim/1/SyncroSim.Console.exe")
#' myLib = ssimLibrary(name="Lib2",aSession=mySession)
#' @name ssimLibrary
# @rdname SSimLibrary-class
#' @export
ssimLibrary <- function(model=NULL,name=NULL,aSession=NULL,backup=F,backupName="backup",backupOverwrite=T,...) new("SSimLibrary",model,name,aSession,...)

setMethod('filepath', signature(x="SSimLibrary"), function(x) x@filepath)

setMethod('session', signature(x="SSimLibrary"), function(x) x@session)

setMethod('info', signature(x="SSimLibrary"), function(x) {
  args = list(list=NULL,library=NULL,lib=filepath(x))
  tt = command(args,session(x))
  out = .dataframeFromSSim(tt,colNames=c("type","value"))
  return(out)
})

setMethod('modelName', signature(x="SSimLibrary"), function(x) {
  #x = myLibrary
  cInfo = info(x)
  out=cInfo$value[cInfo$type=="Primary Module:"]
  return(out)
})

setReplaceMethod(
  f="session",
  signature="SSimLibrary",
  definition=function(x,value){
    if(class(value)!="Session"){
      stop('Must assign a Session object.')
    }
    x@session = value
    return (x)
  }
)





