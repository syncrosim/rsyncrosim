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
    definition=function(.Object,model=NULL,name=NULL,session=NULL,addons=NULL,backup=F,backupName="backup",backupOverwrite=T){
    #model="stsim";name="stsim";session=mySsim;backup=F;backupName="backup";backupOverwrite=T
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
    tt = .dataframeFromSSim(command(args,session),colNames=c("description","value"))


    if(!is.null(model)){
      expectedModule = modelOptions$command[modelOptions$name==model]
      if(!grepl(expectedModule,tt$value[tt$description=="Model Name:"])){
        stop(paste0("A library of that name and a different model type ",tt$value[tt$description=="Model Name:"]," already exists."))
      }
    }

    #addons=c("stsim-ecological-departure", "stsim-stock-flow")
    if(!is.null(addons)){
      addons=gsub(":add-on-transformer","",addons,fixed=T)
      for(i in seq(length.out=length(addons))){
        #i=1
        tt = command(list(create=NULL,addon=NULL,lib=path,name=paste0(addons[i],":add-on-transformer")),session)
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
#' @param addons One or more addons. See addons() for options.
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
          function(model=NULL,name=NULL,session=NULL,addons=NULL,backup=F,backupName="backup",backupOverwrite=T) new("SSimLibrary",model,name,session,addons,backup,backupName,backupOverwrite))

setMethod('filepath', signature(x="SSimLibrary"), function(x) x@filepath)

setMethod('session', signature(x="SSimLibrary"), function(x) x@session)

setMethod('info', signature(x="SSimLibrary"), function(x) {
  args = list(list=NULL,library=NULL,lib=.filepath(x))
  tt = command(args,.session(x))
  out = .dataframeFromSSim(tt,colNames=c("type","value"))
  return(out)
})

#' The name of the primary model associate with a SyncroSim object
#'
#' The name of the primary model associated with a SSimLibarary, Project or Scenario.
#'
#' @param x An object with an associated primary model.
#' @return A model name
#' @export
setGeneric('modelName',function(x) standardGeneric('modelName'))
setMethod('modelName', signature(x="SSimLibrary"), function(x) {
  #x = myLibrary
  cInfo = info(x)
  out=cInfo$value[cInfo$type=="Model Name:"]
  return(out)
})

#' The version of the primary model associated with a SyncroSim object
#'
#' The version of the primary model associated with a SSimLibarary, Project or Scenario.
#'
#' @param x An object with an associated primary model.
#' @return A model version.
#' @export
setGeneric('modelVersion',function(x) standardGeneric('modelVersion'))
setMethod('modelVersion', signature(x="SSimLibrary"), function(x) {
  #x = myLibrary
  cInfo = info(x)
  out=paste(cInfo$type[cInfo$type=="Source Module Version:"],cInfo$value[cInfo$type=="Source Module Version:"])
  return(out)
})

#' Set a SyncroSim session.
#'
#' Set the Session of a SSimLibrary, Project or Scenario object.
#'
#' @param x= A SyncroSim Session.
#' @return An SyncroSim object containing a Session.
#' @examples
#' myLibrary = ssimLibrary()
#' session(myLibrary)=session()
#' session(myLibrary)
#' @export
setGeneric('session<-',function(x,value) standardGeneric('session<-'))
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
#' @param x An SSimLibrary, Project or Scenario associated with a library.
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
setMethod('deleteProjects', signature(x="SSimLibrary"), function(x,project=NULL,...) {
  #x = ssimLibrary(model="stsim", name= "C:/Temp/NewLibrary.ssim",session=devSsim)
  #x = myLibrary
  #project = "TempProject"
  if(is.null(project)){
    if(!class(x)=="Project"){
      stop("Please specify one or more projects to delete.")
    }
    project = id(x)
  }

  if(class(project)=="Project"){
    project=id(project)
  }

  allProjects = projects(x,names=T)
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
    answer <- readline(prompt=paste0("Do you really want to delete project ",allProjects$name[allProjects$id==id],"(",id,")? (y/n): "))
    if(answer=="y"){
      outBit = command(list(delete=NULL,project=NULL,lib=.filepath(x),pid=id,force=NULL),.session(x))
    }else{
      outBit = "skipped"
    }
    if(length(project)==1){
      out = outBit
    }else{
      out[[as.character(cProj)]]=outBit
    }
  }
  return(out)
})

#' Delete scenarios
#'
#' Deletes one or more scenarios from a SyncroSim library.
#'
#' @param x An SSimLibrary, Project or Scenario.
#' @param scenario One or more scenario names or ids.
#' @return A list of "Success!" or failure messages for each scenario.
#' @examples
#' myLibrary = ssimLibrary(model="stsim")
#' myScenario = scenario(project(myLibrary))
#' scenarios(myLibrary,names=T)
#' deleteScenarios(myLibrary,scenario="Scenario")
#' scenarios(myLibrary,names=T)
#' @export
setGeneric('deleteScenarios',function(x,...) standardGeneric('deleteScenarios'))
setMethod('deleteScenarios', signature(x="SSimLibrary"), function(x,scenario=NULL,...) {
  #x = myLibrary
  #scenario = "Scenario"
  if(is.null(scenario)){
    if(!class(x)=="Scenario"){
      stop("Please specify one or more scenarios to delete.")
    }
    scenario = id(x)
  }

  if(class(scenario)=="Scenario"){
    scenario=id(scenario)
  }

  allScenarios = scenarios(x,names=T)
  out = list()
  for(i in seq(length.out=length(scenario))){
    #i = 1
    cScn = scenario[i]
    if(is.character(cScn)){
      id = allScenarios$id[allScenarios$name==cScn]
      if(length(id)>1){
        stop(paste0("Found more than one scenario called ",cScn,". Please specify a scenario id: ",paste(id,collapse=",")))
      }
    }else{
      id = intersect(cScn,allScenarios$id)
    }
    if(length(id)==0){
      print(paste0("Cannot remove the scenario ",cScn," from the library because it does not exist."))
      next
    }
    answer <- readline(prompt=paste0("Do you really want to delete scenario ",allScenarios$name[allScenarios$id==id],"(",id,")? (y/n): "))
    if(answer=="y"){
      outBit = command(list(delete=NULL,scenario=NULL,lib=.filepath(x),sid=id,force=NULL),.session(x))
    }else{
      outBit = "skipped"
    }
    if(length(scenario)==1){
      out = outBit
    }else{
      out[[as.character(cScn)]]=outBit
    }
  }
  return(out)
})


setMethod('scenarios', signature(x="SSimLibrary"), function(x,project=NULL,names=F,results=NULL,...) {
  #x = ssimLibrary(model="stsim", name= "C:/Temp/NewLibrary.ssim",session=devSsim)
  #x = myLibrary;names=T
  #command(list(create=NULL,scenario=NULL,lib=.filepath(x),pid=85,name="Another scenario"),.session(x))
  tt = command(list(list=NULL,scenarios=NULL,lib=.filepath(x)),.session(x))

  if(identical(tt,"Success!")){
    ttFrame = subset(data.frame(id=NA,pid=NA,isResult=NA,name=NA),!is.na(id))
  }else{
    ttFrame=.dataframeFromSSim(tt,colNames=c("id","pid","isResult","name"))
  }
  if(class(x)=="Project"){
    ttFrame = subset(ttFrame,pid==.id(x))
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
    ttList[[ttFrame$id[i]]]=scenario(x,id=ttFrame$id[i],project=as.numeric(ttFrame$pid[i]))
  }
  return(ttList)
})

#' addons of an SSimLibrary
#'
#' The addons of an SSimLibrary.
#'
#' @param x An SSimLibrary, or a Project/Scenario object associated with an SSimLibrary.
#' @param all If T, all available addons are returned. Otherwise, only enabled addons.
#' @return A dataframe of addons.
#' @examples
#' addons(ssimLibrary(model="stsim",name="stsim"))
#' @export
setGeneric('addons',function(x,...) standardGeneric('addons'))
setMethod('addons', signature(x="SSimLibrary"), function(x,all=F) {
  #x = myLibrary
  tt = command(list(list=NULL,addons=NULL,lib=.filepath(x)),.session(x))
  tt = .dataframeFromSSim(tt,colNames=c("status","description","command"))
  if(!all){
    tt=subset(tt,status!="(D)")
  }
  tt$name = gsub(":add-on-transformer","",tt$command,fixed=T)
  return(tt)
})

#' Enable addons.
#'
#' Enable addons of an SSimLibrary, or Project/Scenario with an associated SSimLibrary.
#'
#' @param x= A SSimLibrary, Project or Scenario.
#' @return x
#' @examples
#' myLibrary = ssimLibrary()
#' enableAddons(myLibrary)=c("stsim-ecological-departure", "stsim-stock-flow")
#' addons(myLibrary)
#' @export
setGeneric('enableAddons<-',function(x,value) standardGeneric('enableAddons<-'))
setReplaceMethod(
  f="enableAddons",
  signature="SSimLibrary",
  definition=function(x,value){
    #x=myLibrary
    #value = c("stsim-ecological-departure", "stsim-stock-flow")
    cAdds = addons(x,all=T)
    value=gsub(":add-on-transformer","",value,fixed=T)
    for(i in seq(length.out=length(value))){
      #i=1
      cVal = value[i]
      if(!is.element(cVal,cAdds$name)){
        print(paste0("Warning - ",cVal," is not among the available addons: ",paste(cAdds$name[cAdds$status=="(D)"],collapse=",")))
        next
      }
      cAddsLess = subset(cAdds,status=="(D)")
      if(!is.element(cVal,cAddsLess$name)){
        print(paste0(cVal," is already enabled."))
        next
      }

      tt=command(list(create=NULL,addon=NULL,lib=.filepath(x),name=paste0(cVal,":add-on-transformer")),.session(x))
      if(!identical(tt,"Success!")){print(paste(tt[1],cVal))}
    }

    return (x)
  }
)

#' Disable addons.
#'
#' Disable addons an SSimLibrary, or Project/Scenario with an associated SSimLibrary.
#'
#' @param x= A SSimLibrary, Project or Scenario.
#' @return x
#' @examples
#' myLibrary = ssimLibrary()
#' enableAddons(myLibrary)=c("stsim-ecological-departure")
#' addons(myLibrary)
#' disableAddons(myLibrary)=c("stsim-ecological-departure")
#' addons(myLibrary)
#'
#' @export
setGeneric('disableAddons<-',function(x,value) standardGeneric('disableAddons<-'))
setReplaceMethod(
  f="disableAddons",
  signature="SSimLibrary",
  definition=function(x,value){
    #x=myLibrary
    #value = c("stsim-ecological-departure", "stsim-stock-flow")
    cAdds = addons(x,all=T)
    value=gsub(":add-on-transformer","",value,fixed=T)
    for(i in seq(length.out=length(value))){
      #i=1
      cVal = value[i]
      if(!is.element(cVal,cAdds$name)){
        print(paste0("Warning - ",cVal," is not among the available addons: ",paste(cAdds$name[cAdds$status=="(D)"],collapse=",")))
        next
      }
      cAddsLess = subset(cAdds,status=="(E)")
      if(!is.element(cVal,cAddsLess$name)){
        print(paste0(cVal," is already disabled."))
        next
      }

      tt=command(list(delete=NULL,addon=NULL,force=NULL,lib=.filepath(x),name=paste0(cVal,":add-on-transformer")),.session(x))
      if(!identical(tt,"Success!")){print(paste(tt[1],cVal))}
    }

    return (x)
  }
)
