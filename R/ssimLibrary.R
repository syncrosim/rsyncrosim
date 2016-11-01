# Author: Josie Hughes
# Date : October 2016
# Version 0.1
# Licence GPL v3
#' @include generics.R
#' @include session.R
NULL
#' SyncroSim Library class
#'
#' \code{SSimLibrary} object representing a SyncroSim Library.
#'
#' @seealso See \code{\link{ssimLibrary}} for options when creating or loading an SyncroSim library.
#' @examples
#' # Create or load and query a SyncroSim Library.
#' myLibrary = ssimLibrary(model="stsim")
#' session(myLibrary)
#' filepath(myLibrary)
#' info(myLibrary)
#'
#' # Add or load a project, then get the SyncroSim Library associated with that Project
#' myProject = project(myLibrary)
#' myLibrary = ssimLibrary(myProject)
#'
#' @slot session The SyncroSim session.
#' @slot filepath The path to the library on disk.
#' @name SSimLibrary-class
#' @rdname SSimLibrary-class
#' @export SSimLibrary
SSimLibrary <- setClass("SSimLibrary", representation(session="Session",filepath="character"))
# @name SSimLibrary
# @rdname SSimLibrary-class
setMethod(f="initialize",signature="SSimLibrary",
    definition=function(.Object,model=NULL,name=NULL,session=NULL,backup=F,backupName="backup",backupOverwrite=T){
    #model="stsim";name="stsim";session=mySsim
    #if a syncrosim session is not provided, make one
    if(is.null(session)){
      session = .session()
    }

    modelOptions = models(session)
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
      cStatus = command(args,session)
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
    cStatus = command(args,session)
    if(!is.null(model)){
      expectedModule = modelOptions$description[modelOptions$name==model]
      if(!grepl(expectedModule,cStatus[2])){
        stop(paste0("A library of that name and a different model type ",cStatus[2]," already exists."))
      }
    }
    .Object@session=session
    .Object@filepath=path
    return(.Object)
  }
)
#' @details
#' \itemize{
#'   \item {If model is SyncroSim Project or Scenario: }{Returns the \code{\link{SSimLibrary}} associated with the Project or Scenario.}
#'   \item {If given no name and no model: }{Opens an existing SyncroSim library in
#'   the current working directory - returns an error if more than one library exists. If library does not exist and only one model is installed - creates a library of that type.}
#'   \item {If given a model but no name: }{Opens or creates a library called <model>.ssim in the current working directory.}
#'   \item {If given a name but no model: }{Attempts to open a library of that name. Returns an error if that library does not already exist.}
#'   \item {If given a name and a model: }{Opens or creates a library called <name>.ssim. Returns an error if the library already exists but is a different type of model.}
#' }
# @param model The model type. Optional when loading an existing library.
#' @param name A library file name or library file path. If not a path library is created or opened in the current working directory.
#' @param session A SyncroSim \code{Session}. If NULL, the default SyncroSim Session will be used.
#' @param backup If TRUE, a backup copy is made when an existing library is opened.
#' @param backupName Added to a library filepath to create a backup library.
#' @param backupOverwrite If TRUE, the existing backup of a library (if any) will be overwritten.
#' @return An \code{SSimLibrary} object representing a SyncroSim library.
#' @examples
#' # See the installed models
#' models(session())
#'
#' # Create a library called <model>.ssim in the current working directory.
#' myLibrary = ssimLibrary(model="stsim")
#' session(myLibrary) #The SycroSim session
#' filepath(myLibrary) #Path to the file on disk.
#' info(myLibrary) #Model type and other library information.
#'
#' # Open an existing SyncroSim library in the current working directory - don't make a backup copy.
#' myLibrary = ssimLibrary()
#'
#' # Create a library with a name in the current working directory
#' mySecondLibrary = ssimLibrary(name="Lib2",model="stsim")
#'
#' # Create a library with a name in another directory
#' myThirdLibrary = ssimLibrary(name=paste0(getwd(),"/Temp/Lib3"),model="stsim")
#'
#' # Create or load a library using a specific session
#' mySession = session("C:/Program Files/SyncroSim/1/SyncroSim.Console.exe")
#' myLibrary = ssimLibrary(name="Lib2",session=mySession)
#'
#' # Add a project and get the library associated with that project
#' myProject = project(myLibrary)
#' myLibrary = ssimLibrary(myProject)
#' @name ssimLibrary
# @rdname SSimLibrary-class
setMethod('ssimLibrary',signature(model="missingOrNULLOrChar"),
          function(model=NULL,name=NULL,session=NULL,backup=F,backupName="backup",backupOverwrite=T) new("SSimLibrary",model,name,session,backup,backupName,backupOverwrite))

setMethod('filepath', signature(x="SSimLibrary"), function(x) x@filepath)

setMethod('session', signature(x="SSimLibrary"), function(x) x@session)

setMethod('info', signature(x="SSimLibrary"), function(x) {
  args = list(list=NULL,library=NULL,lib=.filepath(x))
  tt = command(args,.session(x))
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

#' The projects in a SyncroSim library.
#'
#' Get a list of projects in a SyncroSim library.
#'
#' @param x An SSimLibrary object, or a Project or Scenario associated with a Library
#' @param names If FALSE, a list of \code{\link{Project}} objects is returned. If TRUE returns a dataframe containing the name and id of each project.
#' @return By default returns a list of projects identified by the project id. Each element of the list contains a SyncroSim Project object. If names=T, returns a dataframe containing the name and id of each project.
#' @examples
#' myProjects = projects(ssimLibrary(model="stsim",name="stsim"))
#' @export
setGeneric('projects',function(x,...) standardGeneric('projects'))
setMethod('projects', signature(x="SSimLibrary"), function(x,names=F,...) {
  #x = ssimLibrary(model="stsim", name= "C:/Temp/NewLibrary.ssim",session=devSsim)
  #x = myLibrary

  tt = command(list(list=NULL,projects=NULL,lib=.filepath(x)),.session(x))
  if(identical(tt,"Success!")){
    ttFrame = subset(data.frame(id=NA,name=NA),!is.na(id))
  }else{
    ttFrame=.dataframeFromSSim(tt,colNames=c("id","name"))
  }
  if(names){
    return(ttFrame)
  }
  ttList = list()
  for(i in seq(length.out=nrow(ttFrame))){
    #i = 1
    ttList[[ttFrame$id[i]]]=project(x,id=ttFrame$id[i])
  }
  return(ttList)
})

#' Delete projects from a Library
#'
#' Deletes one or more projects from a SyncroSim library.
#'
#' @param x An SSimLibrary object, or a Project or Scenario associated with a Library.
#' @param project One or more project names or ids.
#' @return A list of "Success!" or failure messages for each project.
#' @examples
#' myLibrary = ssimLibrary(model="stsim",session=devSession)
#' myProject = project(myLibrary)
#' projects(myLibrary,names=T)
#' deleteProjects(myLibrary,project="Project1")
#' projects(myLibrary,names=T)
#'
#' @export
setGeneric('deleteProjects',function(x,...) standardGeneric('deleteProjects'))
setMethod('deleteProjects', signature(x="SSimLibrary"), function(x,project,...) {
  #x = ssimLibrary(model="stsim", name= "C:/Temp/NewLibrary.ssim",session=devSsim)
  #x = myLibrary
  #project = "TempProject"

  allProjects = .projects(myLibrary,names=T)
  out = list()
  for(i in seq(length.out=length(project))){
    #i = 1
    cProj = project[i]
    if(is.character(cProj)){
      id = allProjects$id[allProjects$name==cProj]
      if(length(id)>1){
        stop(paste0("The library contains more than one project called ",cProj,". Please specify a project id: ",paste(id,collapse=",")))
      }
    }else{
      id = intersect(cProj,allProjects$id)
    }
    if(length(id)==0){
      print(paste0("Cannot remove the project ",cProj," from the library because it does not exist."))
      next
    }
    outBit = command(list(delete=NULL,project=NULL,lib=.filepath(x),pid=id),.session(x))
    #TO DO: Need console command that does not require additional input. I can ask for confirmation in R.
    if(length(project)==1){
      out = outBit
    }else{
      out[[cProj]]=outBit
    }
  }
  return(out)
})

#' The scenarios in a SyncroSim library or project.
#'
#' Get a list of scenarios in a SSimLibrary or Project.
#'
#' @param x An SSimLibrary or Project object
#' @param names If FALSE, a list of \code{\link{Scenario}} objects is returned. If TRUE returns a dataframe containing the name,id and project id of each scenario.
#' @return By default returns a list of scenarios identified by id. Each element of the list contains a SyncroSim Scenario object. If names=T, returns a dataframe containing the name, id, and project id of each scenario.
#' @examples
#' myScenarios = scenarios(ssimLibrary(model="stsim",name="stsim"))
#' @export
setGeneric('scenarios',function(x,...) standardGeneric('scenarios'))
setMethod('scenarios', signature(x="SSimLibrary"), function(x,names=F,results=NULL,project=NULL,...) {
  #x = ssimLibrary(model="stsim", name= "C:/Temp/NewLibrary.ssim",session=devSsim)
  #x = myLibrary;names=T
  #command(list(create=NULL,scenario=NULL,lib=.filepath(x),pid=85,name="Another scenario"),.session(x))
  tt = command(list(list=NULL,scenarios=NULL,lib=.filepath(x)),.session(x))

  if(identical(tt,"Success!")){
    ttFrame = subset(data.frame(id=NA,pid=NA,isResult=NA,name=NA),!is.na(id))
  }else{
    ttFrame=.dataframeFromSSim(tt,colNames=c("id","pid","isResult","name"))
  }
  if(!is.null(project)){
    if(class(project)=="Project") pid = .id(project)
    if(class(project)=="numeric") pid = project
    if(class(project)=="character"){
      cProjects = projects(x,names=T)
      pid = cProjects$id[cProjects$name==project]
      if(length(pid)>1){
        stop(paste0("There is more than one project called ",project,". Please specify a project id:",paste(pid,collapse=",")))
      }
    }
    cPid
    ttFrame = subset(ttFrame,is.element(pid,cPid))
  }

  if(class(x)=="Project"){
    ttFrame=subset(ttFrame,pid==.id(x))
  }
  if(!is.null(results)){
    if(results){
      ttFrame = subset(ttFrame,isResult=="(Y)")
    }else{
      ttFrame = subset(ttFrame,isResult=="(N)")
    }
  }


  if(names){
    return(ttFrame)
  }
  ttList = list()
  for(i in seq(length.out=nrow(ttFrame))){
    #i = 1
    ttList[[ttFrame$id[i]]]=scenario(x,id=ttFrame$id[i],pid=ttFrame$pid[i])
  }
  return(ttList)
})


