# Author: Josie Hughes
# Date : October 2016
# Version 0.1
# Licence GPL v3
#' @include generics.R
#' Class represents a SyncroSim library.
#'
#' @slot session The SyncroSim session.
#' @slot path The path to the library on disk.
#' @slot info Basic library properties.
#' @name SSimLibrary-class
#' @rdname SSimLibrary-class
#' @exportClass SSimLibrary
SSimLibrary <- setClass("SSimLibrary", representation(session="Session",path="character",info="character"))
#' @name SSimLibrary
#' @rdname SSimLibrary-class
setMethod(f="initialize",signature="SSimLibrary",
    definition=function(.Object,model=NULL,name=NULL,cSession=NULL,...){
    #model="st-sim";name=libName;cSession=NULL

    #TO DO: mapping between model, primaryModule name, and name
    models=list("st-sim"=list(modelCmd="stsim:model-transformer",modelName="ST-Sim State and Transition"))
    if(!is.null(model)){
      if(!is.element(model,names(models))){
        stop(paste("Model type",model,"not recognized. Options are:",paste0(names(models),collapse=",")))
      }
    }

    #If no name is provided, search for a library in the current working directory
    #If there is one library, set name to that library.
    #If there is more than one library, complain.
    #If there are no libraries, set name to model - if model is NULL complain.
    if(is.null(name)){
      fList = list.files(pattern="\\.ssim")
      if(length(fList)>1){
        stop(paste0("The working directory constains more than one SyncroSimLibrary - specify a name:",paste(fList,collapse=",")))
      }
      if(length(fList)==1){
        name = fList[1]
      }
      if(length(fList)==0){
        if(is.null(model)){
          stop(paste0("Please specify a model type for a new library. Options are:",paste(names(models),collapse=",")))
        }
        name=model
      }
    }

    x <- .fullFilename(name)
    if(!grepl(".ssim",x)) x=paste0(x,".ssim")

    #if a syncrosim session is not provided, make one
    if(is.null(cSession)){
      cSession = session()
    }

    #if library does not exist on disk, create it
    if(!file.exists(x)){
      pathBits = strsplit(x,"/")[[1]]
      dir.create(paste(head(pathBits,-1),collapse="/"),showWarnings=F)

      args = list(create=NULL,library=NULL,name=x,model=models[[model]]$modelCmd)
      cStatus = command(args,cSession,...)
      #cStatus=command(args,cSession)
    }
    #ensure the primaryModule specified matches the primaryModule on disk
    args = list(list=NULL,library=NULL,lib=x)
    cStatus = command(args,cSession,...)
    #cStatus= command(args,cSession)
    if(!is.null(model)){
      expectedModule = models[[model]]$modelName
      if(!grepl(expectedModule,cStatus[2])){
        stop(paste0("A library of that name and a different model type ",cStatus[2]," already exists."))
      }
    }
    .Object@session=cSession
    .Object@path=x
    .Object@info=cStatus
    return(.Object)

    return(.Object) # return of the object
  }
)
#' Create or open a SyncroSim library.
#'
#' @details
#' If given no name and no model: Opens an existing SyncroSim library in the
#' current working directory - returns an error if more than one library exists.
#' If given a model but no name: Opens or creates a library called <model>.ssim in the current working directory.
#' If given a name but no model: Attempts to open a library of that name. Returns an error if that library does not already exist.
#' If given a name and a model: Opens or creates a library called <name>.ssim. Returns an error if the library already exists but is a different type of model.
#'
#' @param model=NULL The model type. Optional when loading an existing library.
#' @param name=NULL A library file name or library file path. If not a path library is created or opened in the current working directory.
#' @param cSession A SyncroSim session. If NULL, the default SyncroSim session will be used.
#' @return An SSimLibrary object.
#' @examples
#' #Create a library called <model>.ssim in the current working directory.
#' myLib = ssimLibrary(model="st-sim")
#' session(myLib) #The SycroSim session
#' path(myLib) #Path to the file on disk.
#' info(myLib) #Model type and other library information.
#'
#' #Open an existing SyncroSim library in the current working directory.
#' myLib = ssimLibrary()
#'
#' #Create a library with name in the current working directory
#' myLib2 = ssimLibrary(name="Lib2",model="st-sim")
#'
#' #Create a library with a name and model in another directory
#' myLib3 = ssimLibrary(name=paste0(getwd(),"/Temp/Lib3"),model="st-sim")
#'
#' #create or load a library using a specific session
#' mySession = session("C:/Program Files/SyncroSim/1/SyncroSim.Console.exe")
#' myLib = ssimLibrary(name="Lib2",cSession=mySession)
#' @name ssimLibrary
#' @rdname SSimLibrary-class
#' @export
ssimLibrary <- function(...) new("SSimLibrary", ...)

#' @describeIn path Path to an SSimLibrary on disk.
setMethod('path', signature(x="SSimLibrary"), function(x) x@path)

#' @describeIn session Session from an SSimLibrary.
setMethod('session', signature(x="SSimLibrary"), function(x) x@session)

#' @describeIn info Info about an SSimLibrary.
setMethod('info', signature(x="SSimLibrary"), function(x) x@info)
