# Author: Josie Hughes
# Date : October 2016
# Version 0.1
# Licence GPL v3
#' @include generics.R
#' Class represents a SyncroSim library.
#'
#' @slot path The path to the library on disk.
#' @slot info Basic library properties.
SSimLibrary <- setClass("SSimLibrary", representation(session="Session",path="character",info="character"))
#' Create or open a SyncroSim library.
#'
#' @details
#' Returns an error if the library  existing library \code{ssimLibrary(...)} will return an
#' error if the spe
#'
#' @param name The library name. If not a path the library is assumed to be in the current working directory.
#' @param model The model type. Defaults to an st-sim model.
#' @param console A syncrosim console object. If NULL, the default SyncroSim console will be used.
#' @return An ssimLibrary object.
#' @examples
#' #create or load an st-sim library named exampleLibrary outside the working directory using the default SyncroSim console.
#' libName = paste0(getwd(),"/Temp/exampleLibrary")
#' myLibrary = ssimLibrary(libName)
#'
#' #create or load an st-sim library in the current working directory using the default SyncroSim console.
#' myLibrary = ssimLibrary("exampleLibrary")
#'
#' #create or load a library using a specific console
#' myConsole = syncrosim("C:/Program Files/SyncroSim/1/SyncroSim.Console.exe")
#' myLibrary = ssimLibrary(libName,console=myConsole)
#'
#' #get the library path
#' path(myLibrary)
#' @name ssimLibrary
#' @rdname ssimLibrary-class
#' @export
ssimLibrary <- function(model=NULL,name=NULL,cSession=NULL,...){
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
  return(new("SSimLibrary",session=cSession,path=x,info=cStatus))
}
#' @describeIn path Path to an SSimLibrary on disk.
setMethod('path', signature(x="SSimLibrary"), function(x) x@path)
#' @describeIn session Session from an SSimLibrary.
setMethod('session', signature(x="SSimLibrary"), function(x) x@session)
