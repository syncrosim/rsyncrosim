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
#' @slot datasheetNames The names and scope of all datasheets in the library. Used to speed calculations.
#' @name SSimLibrary-class
#' @rdname SSimLibrary-class
#' @export SSimLibrary
SSimLibrary <- setClass("SSimLibrary", representation(session="Session",filepath="character",datasheetNames="data.frame"))
# @name SSimLibrary
# @rdname SSimLibrary-class
setMethod(f='initialize',signature="SSimLibrary",
    definition=function(.Object,name=NULL,model=NULL,session=NULL,addons=NULL,backup=F,backupName="backup",backupOverwrite=T,forceUpdate=F){
    #model="stsim";name="New Lib";session=NULL;backup=F;backupName="backup";backupOverwrite=T;addons=NULL;forceUpdate=F
    #if a syncrosim session is not provided, make one
    if(is.null(session)){
      session = .session()
    }

    inName = name
    modelOptions = models(session)
    if(!is.null(model)){
      model=gsub(":model-transformer","",model,fixed=T)
      if(!is.element(model,modelOptions$shortName)){
        stop(paste("Model type",model,"not recognized. Options are:",paste0(modelOptions$shortName,collapse=",")))
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
            model = modelOptions$shortName
          }else{
            stop(paste0("Please specify a model type for a new library. Options are:",paste(modelOptions$shortName,collapse=",")))
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

      args = list(create=NULL,library=NULL,name=path,model=modelOptions$name[modelOptions$shortName==model])
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
    #path =.filepath(myLibrary);session=.session(myLibrary)
    args = list(list=NULL,library=NULL,csv=NULL,lib=path)
    tt = command(args,session)
    if(grepl("The library has unapplied updates",tt[[1]])){
      if(is.null(inName)|forceUpdate){answer ="y"}else{
        answer <- readline(prompt=paste0("The library has unapplied updates. Do you want to update ",path,"? (y/n): "))
      }
      if(answer=="y"){
        updateMessage = command(list(update=NULL,lib=path),session)

        if(grepl("Update complete",updateMessage,fixed=T)){
          updateMessage = "Success!"
        }

        if(!identical(updateMessage,"Success!")){
          stop(updateMessage)
        }

      }else{
        stop("Cannot open a library with unapplied updates.")
      }
      tt = command(args,session)
    }
    tt = .dataframeFromSSim(tt)
    if(ncol(tt)<2){
      stop(command(args,session))
    }

    if(!is.null(model)){
      expectedModule = modelOptions$name[modelOptions$shortName==model]
      if(!grepl(expectedModule,tt$value[tt$property=="Model Name:"])){
        stop(paste0("A library of that name and a different model type ",tt$value[tt$property=="Model Name:"]," already exists."))
      }
    }

    #addons=c("stsim-ecological-departure", "stsim-stock-flow")
    if(!is.null(addons)){
      addons=gsub(":add-on-transformer","",addons,fixed=T)

      tt = command(list(list=NULL,addons=NULL,csv=NULL,lib=path),session)
      tt = .dataframeFromSSim(tt)
      cAdds = subset(tt,enabled=="Yes")
      addons=setdiff(addons,gsub(":add-on-transformer","",cAdds$name,fixed=T))

      for(i in seq(length.out=length(addons))){
        #i=1

        tt = command(list(create=NULL,addon=NULL,lib=path,name=paste0(addons[i],":add-on-transformer")),session)
      }
    }
    #args = c("list","datasheets","csv",paste0("lib=",path))
    tt=command(c("list","datasheets","csv",paste0("lib=",path)),session)
    datasheets = .dataframeFromSSim(tt)
    datasheets$dataScope = sapply(datasheets$dataScope,camel)
    if(length(names(datasheets))<4){
      stop("rsyncrosim requires SyncroSim version >= 1.0.37.0")
    }
    names(datasheets) = c("name","displayName","dataScope","isOutput")
    datasheets$isOutput[datasheets$isOutput=="No"]=F
    datasheets$isOutput[datasheets$isOutput=="Yes"]=T
    datasheets$isOutput=as.logical(datasheets$isOutput)
    #datasheets$isSpatial = grepl("Spatial",datasheets$name)&!grepl("NonSpatial",datasheets$name)

    .Object@session=session
    .Object@filepath=path
    .Object@datasheetNames=datasheets
    return(.Object)
  }
)
#' @details
#' \itemize{
#'   \item {If name is SyncroSim Project or Scenario: }{Returns the \code{\link{SSimLibrary}} associated with the Project or Scenario.}
#'   \item {If given no name and no model: }{Opens an existing SyncroSim library in
#'   the current working directory - returns an error if more than one library exists. If library does not exist and only one model is installed - creates a library of that type.}
#'   \item {If given a model but no name: }{Opens or creates a library called <model>.ssim in the current working directory.}
#'   \item {If given a name but no model and name is a valid model type: }{Attempts to open a library of that name. If library does not exist creates a library of type <name> in the current working directory.}
#'   \item {If given a name but no model and name is not a valid model type: }{Attempts to open a library of that name. Returns an error if that library does not already exist.}
#'   \item {If given a name and a model: }{Opens or creates a library called <name>.ssim. Returns an error if the library already exists but is a different type of model.}
#' }
#' @param model The model type. Optional when loading an existing library.
# @param name A library file name or library file path. If not a path library is created or opened in the current working directory.
#' @param session A SyncroSim \code{Session}. If NULL, the default SyncroSim Session will be used.
#' @param addons One or more addons. See addons() for options.
#' @param backup If TRUE, a backup copy is made when an existing library is opened.
#' @param backupName Added to a library filepath to create a backup library.
#' @param backupOverwrite If TRUE, the existing backup of a library (if any) will be overwritten.
#' @param forceUpdate If FALSE (default) user will be prompted to approve any required updates. If TRUE, required updates will be applied silently.
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
setMethod('ssimLibrary',signature(name="missingOrNULLOrChar"),
          function(name=NULL,model=NULL,session=NULL,addons=NULL,backup=F,backupName="backup",backupOverwrite=T,forceUpdate=F) {

    if(is.character(name)){
      if(!grepl(".ssim",name,fixed=T)){
        nameCheck = paste0(name,".ssim")
      }else{
        nameCheck =name
      }
      if(!file.exists(nameCheck)&is.null(model)){
        model=name
      }
    }
    new("SSimLibrary",name,model,session,addons,backup,backupName,backupOverwrite,forceUpdate)
})

setMethod('filepath', signature(x="SSimLibrary"), function(x) x@filepath)

#' @describeIn session Get the Session associated with a SSimLibrary.
setMethod('session', signature(x="SSimLibrary"), function(x) x@session)

setMethod('info', signature(x="SSimLibrary"), function(x) {
  #x=myLibrary
  args = list(list=NULL,library=NULL,csv=NULL,lib=.filepath(x))
  tt = command(args,.session(x))
  out = .dataframeFromSSim(tt,localNames=T)
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
  out=cInfo$value[cInfo$property=="Model Name:"]
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
  out=paste(cInfo$property[cInfo$property=="Source Module Version:"],cInfo$value[cInfo$property=="Source Module Version:"])
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
  f='session',
  signature="SSimLibrary",
  definition=function(x,value){
    if(class(value)!="Session"){
      stop('Must assign a Session object.')
    }
    x@session = value
    return (x)
  }
)

#' Apply updates.
#'
#' Apply updates to a SyncroSim Library.
#'
#' @param x An SSimLibrary object, or a Project or Scenario associated with a Library
#' @return Success or a failure message from the console.
#' @export
setGeneric('update',function(x) standardGeneric('update'))
setMethod('update', signature(x="SSimLibrary"), function(x) {
  #x= myLibrary
  #args = list(update=NULL,lib=.filepath(x));session=.session(x)
  tt = command(list(update=NULL,lib=.filepath(x)),.session(x))
  return(tt[1])
})

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
setGeneric('projects',function(x,names=F) standardGeneric('projects'))
setMethod('projects', signature(x="SSimLibrary"), function(x,names) {
  #x = ssimLibrary(model="stsim", name= "C:/Temp/NewLibrary.ssim",session=devSsim)
  #x = myLibrary

  tt = command(list(list=NULL,projects=NULL,csv=NULL,lib=.filepath(x)),.session(x))
  if(identical(tt,"Success!")){
    ttFrame = data.frame(id=NA,name=NA)
    ttFrame=subset(ttFrame,!is.na(id))
  }else{
    ttFrame=.dataframeFromSSim(tt)
    names(ttFrame)[names(ttFrame)=="iD"]="id"
  }

  if(names){
    return(ttFrame)
  }
  ttList = list()
  for(i in seq(length.out=nrow(ttFrame))){
    #i = 1
    ttList[[as.character(ttFrame$id[i])]]=project(x,id=ttFrame$id[i],create=F,projects=ttFrame)
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
setGeneric('deleteProjects',function(x,project=NULL,force=F) standardGeneric('deleteProjects'))
setMethod('deleteProjects', signature(x="SSimLibrary"), function(x,project,force) {
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
    if(force){
      answer="y"
    }else{
      answer <- readline(prompt=paste0("Do you really want to delete project ",allProjects$name[allProjects$id==id],"(",id,")? (y/n): "))
    }
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
#' @param force If FALSE (default) user is prompted to confirm deletions.
#' @return A list of "Success!" or failure messages for each scenario.
#' @examples
#' myLibrary = ssimLibrary(model="stsim")
#' myScenario = scenario(project(myLibrary))
#' scenarios(myLibrary,names=T)
#' deleteScenarios(myLibrary,scenario="Scenario")
#' scenarios(myLibrary,names=T)
#' @export
setGeneric('deleteScenarios',function(x,scenario=NULL,force=FALSE) standardGeneric('deleteScenarios'))
setMethod('deleteScenarios', signature(x="SSimLibrary"), function(x,scenario,force) {
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
    if(force){
      answer="y"
    }else{
      answer <- readline(prompt=paste0("Do you really want to delete scenario ",allScenarios$name[allScenarios$id==id],"(",id,")? (y/n): "))
    }
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

setMethod('scenarios', signature(x="SSimLibrary"), function(x,project,names,results,select) {
  #x = ssimLibrary(model="stsim", name= "C:/Temp/NewLibrary.ssim",session=devSsim)
  #x = myLibrary;names=T
  #command(list(create=NULL,scenario=NULL,lib=.filepath(x),pid=85,name="Another scenario"),.session(x))
  tt = command(list(list=NULL,scenarios=NULL,csv=NULL,lib=.filepath(x)),.session(x))
  ttFrame=.dataframeFromSSim(tt,localNames=T)
  names(ttFrame)[names(ttFrame)=="scenarioID"]="id"
  names(ttFrame)[names(ttFrame)=="projectID"]="pid"

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
    cPid = pid
    ttFrame = subset(ttFrame,is.element(pid,cPid))
  }

  if(class(x)=="Project"){
    ttFrame=subset(ttFrame,pid==.id(x))
  }
  if(!is.null(results)){
    if(results){
      ttFrame = subset(ttFrame,isResult=="Yes")
    }else{
      ttFrame = subset(ttFrame,isResult=="No")
    }
  }
  if(!is.null(select)){
    #subset=c(1,2,3)
    if(is.numeric(select)){
      ttFrame=subset(ttFrame,is.element(id,select))
    }else{
      ttFrame = subset(ttFrame,is.element(name,select))
    }
  }

  if(names){
    #ttFrame=allScns
    ttFrame$parentName = sapply(ttFrame$name,.getParentName,simplify=T)
    return(ttFrame)
  }
  ttList = list()
  for(i in seq(length.out=nrow(ttFrame))){
    #i = 1
    ttList[[as.character(ttFrame$id[i])]]=scenario(x,id=ttFrame$id[i],project=as.numeric(ttFrame$pid[i]),create=F,scenarios=ttFrame)
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
setGeneric('addons',function(x,all=F) standardGeneric('addons'))
setMethod('addons', signature(x="SSimLibrary"), function(x,all) {
  #x = myLibrary
  tt = command(list(list=NULL,addons=NULL,csv=NULL,lib=.filepath(x)),.session(x))
  tt = .dataframeFromSSim(tt)
  if(!all){
    tt=subset(tt,enabled=="Yes")
  }
  tt$shortName = gsub(":add-on-transformer","",tt$name,fixed=T)
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
  f='enableAddons',
  signature="SSimLibrary",
  definition=function(x,value){
    #x=myLibrary
    #value = c("stsim-ecological-departure", "stsim-stock-flow")
    cAdds = addons(x,all=T)
    value=gsub(":add-on-transformer","",value,fixed=T)
    for(i in seq(length.out=length(value))){
      #i=1
      cVal = value[i]
      if(!is.element(cVal,cAdds$shortName)){
        print(paste0("Warning - ",cVal," is not among the available addons: ",paste(cAdds$shortName[cAdds$enabled=="No"],collapse=",")))
        next
      }
      cAddsLess = subset(cAdds,enabled=="No")
      if(!is.element(cVal,cAddsLess$shortName)){
        print(paste0(cVal," is already enabled."))
        next
      }

      tt=command(list(create=NULL,addon=NULL,lib=.filepath(x),name=paste0(cVal,":add-on-transformer")),.session(x))
      if(!identical(tt,"Success!")){print(paste(tt[1],cVal))}
    }

    x@datasheetNames = .datasheets(x,scope="all",refresh=T)
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
  f='disableAddons',
  signature="SSimLibrary",
  definition=function(x,value){
    #x=myLibrary
    #value = c("stsim-ecological-departure", "stsim-stock-flow")
    cAdds = addons(x,all=T)
    value=gsub(":add-on-transformer","",value,fixed=T)
    for(i in seq(length.out=length(value))){
      #i=1
      cVal = value[i]
      if(!is.element(cVal,cAdds$shortName)){
        print(paste0("Warning - ",cVal," is not among the available addons: ",paste(cAdds$shortName[cAdds$enabled=="No"],collapse=",")))
        next
      }
      cAddsLess = subset(cAdds,enabled=="Yes")
      if(!is.element(cVal,cAddsLess$shortName)){
        print(paste0(cVal," is already disabled."))
        next
      }

      tt=command(list(delete=NULL,addon=NULL,force=NULL,lib=.filepath(x),name=paste0(cVal,":add-on-transformer")),.session(x))
      if(!identical(tt,"Success!")){print(paste(tt[1],cVal))}
    }

    x@datasheetNames = .datasheets(x,scope="all",refresh=T)
    return (x)
  }
)

setMethod('datasheets', signature(x="SSimLibrary"), function(x,project,scenario,names,scope,optional,empty,lookupsAsFactors,refresh) {
  #x = myLibrary;project=1;scenario=NULL;names=T;empty=T;scope="project"
  x = .getFromXProjScn(x,project,scenario)

  #command(c("export","datasheet","help"),.session(myLibrary))
  #Get datasheet dataframe
  if(!refresh){
    datasheets=x@datasheetNames
  }else{
    #x=myLibrary
    tt=command(c("list","datasheets","csv",paste0("lib=",.filepath(x))),.session(x))
    if(grepl("The library has unapplied updates",tt[[1]])){
      stop(tt)
    }
    datasheets = .dataframeFromSSim(tt)
    datasheets$dataScope = sapply(datasheets$dataScope,camel)
    names(datasheets) = c("name","displayName","dataScope","isOutput")
    datasheets$isOutput[datasheets$isOutput=="No"]=F
    datasheets$isOutput[datasheets$isOutput=="Yes"]=T
    datasheets$isOutput=as.logical(datasheets$isOutput)
    #datasheets$isSpatial = grepl("Spatial",datasheets$name)&!grepl("NonSpatial",datasheets$name)
    #TO DO - export this info from SyncroSim
  }
  if(!is.null(scope)&&(scope=="all")){
    return(datasheets)
  }
  if(is.element(class(x),c("Project","SSimLibrary"))){
    datasheets = subset(datasheets,dataScope!="scenario")
  }
  if(is.element(class(x),c("SSimLibrary"))){
    datasheets = subset(datasheets,dataScope!="project")
  }
  if(!is.null(scope)){
    if(!is.element(scope,c("scenario","project","library"))){
      stop("Invalid scope ",scope,". Valid scopes are 'scenario', 'project', 'library' and NULL.")
    }
    datasheets=subset(datasheets,dataScope==scope)
  }
  if(names){
    return(datasheets)
  }

  ttList = list()
  for(i in seq(length.out=nrow(datasheets))){
    #i = 1
    ttList[[datasheets$name[i]]]=datasheet(x,name=datasheets$name[i],optional=optional,empty=empty,lookupsAsFactors=lookupsAsFactors)
  }
  return(ttList)
})

setMethod('datasheet', signature(x="SSimLibrary"), function(x,name,project,scenario,optional,empty,lookupsAsFactors,sqlStatements,includeKey) {
  #x = myResult;project=NULL;scenario=NULL;name="STSim_SecondaryStratum";optional=F;empty=F;lookupsAsFactors=T;sqlStatements=list(select="SELECT *",groupBy="")

  allProjects=NULL;allScns=NULL
  passScenario = scenario;passProject = project
  if((length(project)>0)){
    passProject = NULL
    pid=project
    if(class(project[[1]])=="Project"){
      pid = names(project)
    }
    if(class(project[[1]])=="character"){
      #x=myLibrary
      allProjects = projects(x,names=T)
      pid = allProjects$id[is.element(name,project)]
    }
  }
  if((length(scenario)>0)){
    passScenario = NULL
    sid=scenario
    if(class(scenario[[1]])=="Scenario"){
      sid = names(scenario)
    }
    if(class(scenario[[1]])=="character"){
      allScns = scenarios(x,names=T)
      sid = allScenarios$id[is.element(name,scenario)]
    }
  }

  x = .getFromXProjScn(x,passProject,passScenario)
  if(is.null(scenario)||(length(scenario)==1)){
    if(class(x)=="Scenario"){sid=.id(x)}else{sid=scenario}
  }
  if(is.null(project)||(length(project)==1)){
    pid=project
    if(class(x)=="Scenario"){pid=.pid(x)}
    if(class(x)=="Project"){pid=.id(x)}
    if((class(x)=="SSimLibrary")&(length(scenario)>0)){
    }
  }

  if(!includeKey){
    rmId = strsplit(name,"_")[[1]][2]
    rmCols = c(paste0(rmId,"ID"))
  }else{
    rmCols=c()
  }

  dir.create(paste0(dirname(.filepath(x)),"/Temp"), showWarnings = FALSE)

  cName = name
  datasheetNames = .datasheets(x,scope="all")
  sheetNames= subset(datasheetNames,name==cName)
  if(nrow(sheetNames)==0){
    datasheetNames = .datasheets(x,scope="all",refresh=T)
    sheetNames= subset(datasheetNames,name==cName)
    if(nrow(sheetNames)==0){
      stop("Datasheet ",name," not found in library.")
    }
  }

  useConsole=F
  tempFile = paste0(dirname(.filepath(x)),"/Temp/",name,".csv")

  if(!empty){
    #Only query database if output or multiple scenarios/project or complex sql

    useConsole = (!sheetNames$isOutput)
    useConsole = useConsole&((sqlStatements$select=="SELECT *"))#&(!lookupsAsFactors))
    useConsole = useConsole&!((sheetNames$dataScope=="project")&(length(pid)>1))
    useConsole = useConsole&!((sheetNames$dataScope=="scenario")&(length(sid)>1))


    if(useConsole){

      #command(c("export","datasheet","help"),.session(x))

      unlink(tempFile)

      args =list(export=NULL,lib=.filepath(x),sheet=name,file=tempFile,allsheets=NULL,force=NULL,includepk=NULL)#filepath=NULL

      if(sheetNames$dataScope=="project"){args[["pid"]]=pid}

      if(is.element(sheetNames$dataScope,c("project","scenario"))){args[["pid"]]=pid}
      if(sheetNames$dataScope=="scenario"){args[["sid"]]=sid}

      tt=command(args,.session(x))

      if(!identical(tt,"Success!")){
        stop(tt)
      }

      #TO DO: think about multithreading - ensure no possibility of overwriting the transient file
      sheet = read.csv(tempFile,as.is=T)
      unlink(tempFile)
      #print("used console")
    }else{
      #query database directly if necessary
      #install.packages("RSQLite");library(RSQLite)
      #x=myLibrary;name="STSim_OutputStratumState";sid=c(6)

      drv = DBI::dbDriver('SQLite')
      con = DBI::dbConnect(drv,.filepath(x))
      #fields = DBI::dbListFields(con,name)

      sqlStatements$where = ""
      sqlStatements$from = paste("FROM",name)
      if(sheetNames$dataScope=="scenario"){
        if(is.null(sid)){
          stop("Specify a scenario.")
        }else{
          #following http://faculty.washington.edu/kenrice/sisg-adv/sisg-09.pdf
          #and http://www.sqlitetutorial.net/sqlite-in/
          sqlStatements$where = paste0("WHERE ScenarioID IN (",paste(sid,collapse=","),")")
        }
      }
      if(sheetNames$dataScope=="project"){
        if(is.null(pid)){
          stop("Specify a project.")
        }else{
          sqlStatements$where = paste0("WHERE ProjectID IN (",paste(pid,collapse=","),")")
        }
      }
      #  sheet = DBI::dbReadTable(con, name)
      sql = paste(sqlStatements$select,sqlStatements$from,sqlStatements$where,sqlStatements$groupBy)
      #sql = paste("SELECT ScenarioID,Iteration,Timestep,StratumID,SecondaryStratumID,StateClassID,StateLabelXID,StateLabelYID, SUM(Amount)",sqlStatements$from,sqlStatements$where,sqlStatements$groupBy)
      sheet = DBI::dbGetQuery(con,sql)
      DBI::dbDisconnect(con)
    }
  }else{
    sheet=data.frame(temp=NA)
  }
  if(nrow(sheet)>0){
    sheet[sheet==""]=NA
  }
  if(empty|lookupsAsFactors){
    tt=command(c("list","columns","csv",paste0("lib=",.filepath(x)),paste0("sheet=",name)),.session(x))
    sheetInfo = .dataframeFromSSim(tt)
    sheetInfo$id = seq(length.out=nrow(sheetInfo))
    sheetInfo = subset(sheetInfo,!is.element(name,rmCols))

    if(!optional){
      if(!empty){
        sheetInfo$Optional[is.element(sheetInfo$name,colnames(sheet))]="Present"
      }
      sheetInfo = subset(sheetInfo,is.element(optional,c("No","Present")))
    }
    sheetInfo = sheetInfo[order(sheetInfo$id),]

    if(nrow(sheet)==0){
      sheet[1,1]=NA
    }

    outNames = c()

    directQuery=F
    if(lookupsAsFactors&!useConsole){
      directQuery = (length(pid)>1)|(length(sid)>1)
      #TO DO: must export IDs in lookup tables.
      if(directQuery){
        drv = DBI::dbDriver('SQLite')
        con = DBI::dbConnect(drv,.filepath(x))
        #console export can't handle multiple scenarios/projects - so query database directly
      }else{
        tempFile = paste0(dirname(.filepath(x)),"/Temp/",name,".csv")
        args =list(export=NULL,lib=.filepath(x),sheet=name,file=tempFile,allsheets=NULL,force=NULL,valonly=NULL,includepk=NULL)
        if(sheetNames$dataScope=="project"){args[["pid"]]=pid}
        if(is.element(sheetNames$dataScope,c("project","scenario"))){args[["pid"]]=pid}
        if(sheetNames$dataScope=="scenario"){args[["sid"]]=sid}
        tt=command(args,.session(x))
        if(!identical(tt,"Success!")){
          stop(tt)
        }
        #command(c("export","sheet","help"))
      }
    }
    for(i in seq(length.out=nrow(sheetInfo))){
      #i =6
      cRow = sheetInfo[i,]
      if(!is.element(cRow$name,colnames(sheet))){
        if(sqlStatements$select=="SELECT *"){
          sheet[[cRow$name]] = NA
        }else{
          next
        }
      }
      outNames = c(outNames,cRow$name)
      if((is.element(cRow$type,c("Integer","Double","Single")))&!is.element(cRow$valType,c("DataSheet","List"))){
        sheet[[cRow$name]] = as.numeric(sheet[[cRow$name]])
      }
      if(cRow$type=="String"){
        sheet[[cRow$name]] = as.character(sheet[[cRow$name]])
      }
      if(cRow$type=="Boolean"){
        if(length(setdiff(unique(sheet[[cRow$name]]),c(NA)))>0){
          sheet[[cRow$name]] = gsub("Yes","1",sheet[[cRow$name]])
          sheet[[cRow$name]] = gsub("No","0",sheet[[cRow$name]])
          sheet[[cRow$name]]=as.logical(abs(as.numeric(sheet[[cRow$name]])))
          #stop("handle this case")
        }
      }
      if((cRow$valType=="List")&lookupsAsFactors){
        opts = cRow$formula1
        opts = strsplit(opts,"|",fixed=T)[[1]]
        cLevels = c()
        for(j in seq(length.out=length(opts))){
          cLevels=c(cLevels,strsplit(opts[j],":",fixed=T)[[1]][2])
        }
        sheet[[cRow$name]] = factor(sheet[[cRow$name]],levels=cLevels)
      }
      if(cRow$valType=="DataSheet"){
        if(lookupsAsFactors){
          #if a number, ignore - SyncroSim will do the checking
          #if(!identical(cRow$formula1,suppressWarnings(as.character(as.numeric(cRow$formula1))))){
          #console export can't handle multiple projects/scenarios - so query database directly if necessary.
          if(directQuery){
            lookupSheet =   DBI::dbReadTable(con, name=cRow$formula1)
          }else{
            lookupPath = gsub(name,cRow$formula1,tempFile,fixed=T)
            if(!file.exists(lookupPath)){
              lookupSheet=data.frame(Name=NULL)
            }else{
              lookupSheet = read.csv(lookupPath,as.is=T)
            }
          }
          if(is.element("ProjectID",names(lookupSheet))){
            if(identical(pid,NULL)&!identical(sid,NULL)){
              if(is.null(allScns)){
                allScns = scenarios(x,names=T)
              }
              findPrjs = allScns$pid[is.element(allScns$id,sid)]
            }else{
              findPrjs = pid
            }
            lookupSheet = subset(lookupSheet,is.element(ProjectID,pid))
          }
          if(is.element("ScenarioID",names(lookupSheet))){
            if(!is.null(sid)){
              lookupSheet=subset(lookupSheet,is.element(ScenarioID,sid))
            }
          }
          #lookupSheet = datasheet(x,project=findPrjs,scenario=sid,name=cRow$formula1,lookupsAsFactors=F)
          if((nrow(lookupSheet)==0)&(cRow$optional=="No")){
            if(!grepl("Output",name)){
              warning(paste0(cRow$name," depends on ",cRow$formula1,". You should load ",cRow$formula1," before setting ",name,"."))
            }
          }
          lookupSheet=lookupSheet[order(lookupSheet[[names(lookupSheet[1])]]),]
          lookupLevels = lookupSheet$Name
          if(is.numeric(sheet[[cRow$name]])){
            if(nrow(lookupSheet)>0){
              if(length(intersect("Name",names(lookupSheet)))==0){
                stop("Something is wrong. Expecting Name in lookup table.")
              }
              #if(is.element(cRow$name,names(lookupSheet))){
              lookupMerge = subset(lookupSheet,select=c(names(lookupSheet)[1],"Name"))
              #}else{
              #  lookupMerge = subset(lookupSheet,select=c("ID","Name"))
              #}

              names(lookupMerge) = c(cRow$name,"lookupName")
              sheet=merge(sheet,lookupMerge, all.x=T)
              sheet[[cRow$name]]=sheet$lookupName
              sheet$lookupName=NULL
            }
          }
          sheet[[cRow$name]]=factor(sheet[[cRow$name]],levels=lookupLevels)
          #TO DO: handle formula1/formula2
        }else{
          sheet[[cRow$name]]=as.character(sheet[[cRow$name]])
        }
      }
      if(cRow$formula2!="N/A"){
        stop("handle this case")
      }
    }
    if(lookupsAsFactors&&!useConsole&&directQuery){
      DBI::dbDisconnect(con)
    }
    rmSheets = unique(sheetInfo$formula1[sheetInfo$valType=="DataSheet"])
    for(i in seq(length.out=length(rmSheets))){
      unlink(gsub(name,rmSheets[i],tempFile,fixed=T))
    }

    #TO DO: deal with NA values in sheet
    #put columns in correct order
    sheet$colOne = sheet[,1]
    sheet$cOrder=seq(1,nrow(sheet))
    if(empty){
      sheet= subset(sheet,is.null(colOne))
    }else{
      sheet=subset(sheet,!is.na(colOne))
      sheet = sheet[order(sheet$cOrder),]
    }
    sheet = subset(sheet,select=outNames)
  }

  if(is.element("ProjectID",names(sheet))){
    if(length(pid)==1){
      sheet$ProjectID = NULL
    }else{
      if(nrow(sheet)>0){
        if(is.null(allProjects)){
          allProjects = projects(x,names=T)
        }
        names(allProjects) = c("ProjectID","ProjectName")
        sheet=merge(allProjects,sheet,all.y=T)
      }
    }
  }

  if(is.element("ScenarioID",names(sheet))){
    if(length(sid)==1){
      sheet$ScenarioID = NULL
    }else{
      if(nrow(sheet)>0){
        if(is.null(allScns)){
          allScns = scenarios(x,names=T)
        }
        allScns=subset(allScns,select=c(id,pid,name,parentName))
        names(allScns) = c("ScenarioID","ProjectID","ScenarioName","ScenarioParent")
        sheet=merge(allScns,sheet,all.Y=T)
      }
    }
  }

  return(sheet)
})

setMethod('loadDatasheets', signature(x="SSimLibrary"), function(x,data,name,project,scenario,breakpoint) {
  #x = myScenario;project=NULL;scenario=NULL;name=sheetName;data=mySheet;breakpoint=T
  x = .getFromXProjScn(x,project,scenario)

  sheetNames = datasheets(x)

  if(class(data)=="data.frame"){
    if(is.null(name)){
      stop("Need a datasheet name.")
    }
    hdat = data
    data = list()
    data[[name]]=hdat
  }

  if((class(data)!="list")||(class(data[[1]])!="data.frame")){
    stop("data must be a dataframe or list of dataframes.")
  }
  out=list()
  for(i in seq(length.out=length(data))){
    #i=1
    cName = names(data)[i]
    cDat = data[[cName]]
    for(j in seq(length.out=ncol(cDat))){
      if(is.factor(cDat[[j]])){cDat[[j]]=as.character(cDat[[j]])}
      if(is.logical(cDat[[j]])){
        inCol = cDat[[j]]
        cDat[[j]][inCol]="Yes";cDat[[j]][!inCol]="No"
      }
    }
    cDat[is.na(cDat)]=""

    if(breakpoint){pathBit = paste0(.filepath(x),'.temp/Data')}else{pathBit = paste0(dirname(.filepath(x)),'/Temp')}

    dir.create(pathBit, showWarnings = FALSE,recursive=T)
    tempFile = paste0(pathBit,"/",cName,".csv")

    write.csv(cDat,file=tempFile,row.names=F,quote=F)
    if(breakpoint){
      out[[cName]] = "Success!"
      next
    }

    args = list(import=NULL,lib=.filepath(x),sheet=cName,file=tempFile)
    scope =sheetNames$dataScope[sheetNames$name==cName]
    if(length(scope)==0){
      sheetNames = datasheets(x,refresh=T)
      scope =sheetNames$dataScope[sheetNames$name==cName]
      if(length(scope)==0){
        stop("name not found in datasheetNames")
      }
    }

    if(class(x)=="Project"){
      if(scope=="project"){args[["pid"]]=.id(x)}
    }
    if(class(x)=="Scenario"){
      if(is.element(scope,c("project","scenario"))){args[["pid"]]=.pid(x)}
      if(scope=="scenario"){args[["sid"]]=.id(x)}
    }
    tt=command(args,.session(x))
    unlink(tempFile)
    out[[cName]] = tt
  }
  return(out)
})

setMethod('run', signature(x="SSimLibrary"), function(x,scenario,onlyIds,jobs) {
  #x=myScenario;scenario="Harvest"
  #command(c("run","help"),.session(x))


  if(is.null(scenario)){
    if(class(x)!="Scenario"){
      stop("Need a scenario to run.")
    }
    scenario = .id(x)
  }
  #name(x)
  scenarios=NULL
  out=list()
  for(i in seq(length.out=length(scenario))){
    #i =1
    cScn = scenario[[i]]
    inScn = as.character(cScn)
    if(!identical(inScn,suppressWarnings(as.character(as.numeric(cScn))))){
      if(is.null(scenarios)){
        scenarios = .scenarios(x,names=T)
      }
      findScn = subset(scenarios,name==cScn)
      if(nrow(findScn)==0){
        stop("Scenario ",cScn," not found.")
      }
      if(nrow(findScn)>1){
        stop("There is more than one scenario named ",cScn,". Provide a scenario id: ",paste(findScn$id,collapse=","))
      }
      cScn = as.numeric(findScn$id)
    }else{
      cScn = as.numeric(cScn)
    }
    #now assume we have a single scenario
    if(!is.numeric(cScn)){stop("Something is wrong.")}

    print(paste0("Running scenario ",inScn,"..."))

    #x=myScenario
    if(class(x)=="Scenario"){
      breakpoints = breakpoints(x)
    }else{
      breakpoints=NULL
    }
    if((class(breakpoints)!="list")|(length(breakpoints)==0)){
      #TO DO: handle jobs, transformer and inpl.
      tt = command(list(run=NULL,lib=.filepath(x),sid=cScn,jobs=jobs),.session(x))

      resultId = strsplit(tt,": ",fixed=T)[[1]][2]
    }else{
      #x=myScenario;jobs=2
      # devtools::document();devtools::load_all()

      # create a session
      cBreakpointSession=breakpointSession(x)
      #TO DO: multiple tries in connection

      # load a library
      msg =paste0('load-library --lib=\"',filepath(x),'\"')
      ret=remoteCall(cBreakpointSession,msg)
      if(ret!="NONE"){
        stop("Something is wrong: ",ret)
      }

      # set breakpoints
      ret = setBreakpoints(cBreakpointSession)
      if(ret!="NONE"){
        stop("Something is wrong: ",ret)
      }

      #resultId=ret
      resultId = run(cBreakpointSession,jobs=jobs)

      resp = writeLines("shutdown", connection(cBreakpointSession),sep = "")
      close(connection(cBreakpointSession)) # Close the connection.
      cBreakpointSession=NULL
    }
    if(!identical(resultId,suppressWarnings(as.character(as.numeric(resultId))))){
      out[[inScn]]=tt
      print(tt)
    }else{
      if(onlyIds){
        out[[inScn]] = as.numeric(resultId)
      }else{
        out[[inScn]] = .scenario(x,id=as.numeric(resultId))
      }
    }

  }
  return(out)
})
