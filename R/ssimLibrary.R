# Author: Josie Hughes
# Date : October 2016
# Version 0.1
# Licence GPL v3
#' @include generics.R
#' @include AAAClassDefinitions.R
NULL
# @name SsimLibrary
# @rdname SsimLibrary-class
setMethod(f='initialize',signature="SsimLibrary",
    definition=function(.Object,name=NULL,model=NULL,session=NULL,addon=NULL,forceUpdate=F,create=T){
    #name=libPath;model=NULL;session=NULL;addon=NULL;forceUpdate=forceUpdate;create=T
    #if a syncrosim session is not provided, make one
    if(is.null(session)){
      session = .session()
    }
    if(is.character(session)){
      session=.session(session)
    }

    inName = name
    inModel=model
    if(is.null(name)){
      name="SsimLibrary.ssim"
    }
    
    if(is.null(model)){
      model=defaultModel(session) #assume validity of session object has already been checked.
    }else{
      modelOptions = models(session)
      model=gsub(":model-transformer","",model,fixed=T)
      if(!is.element(model,modelOptions$shortName)){
        stop(paste("Model type",model,"not recognized. Options are:",paste0(modelOptions$shortName,collapse=",")))
      }
    }
    
    path <- .fullFilename(name)
    if(!grepl(".ssim",path)) path=paste0(path,".ssim")

    #if library does not exist on disk, create it
    if(!file.exists(path)){
      if(!create){
        stop(paste0("Library not found: ",path))
      }
      if(is.null(model)){
        stop('Specify a model for the new library.')
      }
      pathBits = strsplit(path,"/")[[1]]
      dir.create(paste(head(pathBits,-1),collapse="/"),showWarnings=F)
      
      if(!exists("modelOptions")){
        modelOptions = models(session)
      }

      args = list(create=NULL,library=NULL,name=path,model=modelOptions$name[modelOptions$shortName==model])
      cStatus = command(args,session)
    }#else{
      #x="C:/Temp/NewLibrary.ssim"
      #if(backup){
      #  backupName = gsub(".ssim",paste0("_",backupName,".ssim"),path,fixed=T)
      #  if(file.exists(backupName)&!backupOverwrite){
      #    stop(paste0('Backup ',backupName,' already exists. Set backupOverwrite=T or provide a different backupName.'))
      #  }
      #  file.copy(path, backupName, overwrite=T)
      #}
    #}
    #ensure the primaryModule specified matches the primaryModule on disk
    #path =.filepath(myLibrary);session=.session(myLibrary)
    args = c("list","datasheets","csv",paste0("lib=",path))
    tt=command(args,session)
    if(grepl("The library has unapplied updates",tt[[1]])){
      if(is.null(inName)|forceUpdate){answer ="y"}else{
        answer <- readline(prompt=paste0("The library has unapplied updates. Do you want to update ",path,"? (y/n): "))
      }
      if(answer=="y"){
        updateMessage = command(list(update=NULL,lib=path),session)

        if(grepl("Update complete",updateMessage,fixed=T)){
          updateMessage = "saved"
        }

        if(!identical(updateMessage,"saved")){
          stop(updateMessage)
        }

      }else{
        stop("Cannot open a library with unapplied updates.")
      }
      tt = command(args,session)
    }
    datasheets = .dataframeFromSSim(tt,convertToLogical=c("isOutput","isSingle"))
    datasheets$scope = sapply(datasheets$scope,camel)
    #datasheets$isSpatial = grepl("Spatial",datasheets$name)&!grepl("NonSpatial",datasheets$name)

    if(!is.null(inModel)){
      args = list(list=NULL,library=NULL,csv=NULL,lib=path)
      tt = command(args,session)
      tt = .dataframeFromSSim(tt)
      if(ncol(tt)<2){
        stop(command(args,session))
      }
      
      if(!exists("modelOptions")){modelOptions=models(session)}
      expectedModule = modelOptions$name[modelOptions$shortName==model]
      if(!grepl(expectedModule,tt$value[tt$property=="Model Name:"])){
        stop(paste0("A library of that name and a different model type ",tt$value[tt$property=="Model Name:"]," already exists."))
      }
    }

    #addons=c("stsim-ecological-departure", "stsim-stock-flow")
    if(!is.null(addon)){
      addon=gsub(":add-on-transformer","",addon,fixed=T)

      tt = command(list(list=NULL,addons=NULL,csv=NULL,lib=path),session)
      tt = .dataframeFromSSim(tt)
      cAdds = subset(tt,enabled=="Yes")
      addon=setdiff(addon,gsub(":add-on-transformer","",cAdds$name,fixed=T))

      for(i in seq(length.out=length(addon))){
        #i=1

        tt = command(list(create=NULL,addon=NULL,lib=path,name=paste0(addon[i],":add-on-transformer")),session)
      }
    }

    .Object@session=session
    .Object@filepath=path
    .Object@datasheetNames=datasheets
    return(.Object)
  }
)

setGeneric('.ssimLibrary',function(name=NULL,model=NULL,session=NULL,addon=NULL,forceUpdate=F,create=F) standardGeneric('.ssimLibrary'))
setMethod('.ssimLibrary',signature(name="missingOrNULLOrChar"),
          function(name,model,session,addon,forceUpdate,create) {
            new("SsimLibrary",name,model,session,addon,forceUpdate,create)
          })
setMethod('.ssimLibrary', signature(name="SsimLibrary"), function(name,model,session,addon,forceUpdate,create) {
  #model=cScn
  if(class(name)=="SsimLibrary"){
    out=name
  }else{
    out = .ssimLibrary(name=.filepath(name),session=.session(name),create=F)
  }
  return(out)
})

#' Create or open a library.
#'
#' Creates or opens an \code{\link{SsimLibrary}} object representing a SyncroSim library.
#'
#' @param name Character string, Project/Scenario. A file name or SyncroSim Project or Scenario. Optional.
#' @export
setGeneric('ssimLibrary',function(name=NULL,...) standardGeneric('ssimLibrary'))

setMethod('ssimLibrary', signature(name="SsimLibrary"), function(name) {
  #model=cScn
  if(class(name)=="SsimLibrary"){
    out=name
  }else{
    out = .ssimLibrary(name=.filepath(name),session=.session(name),create=F)
  }
  return(out)
})

#' @details
#' 
#' \itemize{
#'   \item {If name is SyncroSim Project or Scenario: }{Returns the \code{\link{SsimLibrary}} associated with the Project or Scenario.}
#'   \item {If name is NULL: }{Create/open a SsimLibrary in the current working directory with the filename SsimLibrary.ssim.}
#'   \item {If name is a string: }{If string is not a valid path treat as filename in working directory. If no file suffix provided in string then add .ssim. Attempts to open a library of that name. If library does not exist creates a library of type model in the current working directory.}
#'   \item {If given a name and a model: }{Create/open a library called <name>.ssim. Returns an error if the library already exists but is a different type of model.}
#' }
#' @param model Character. The model type. If NULL, defaultModel(session()) will be used.
#' @param session Session. If NULL, session() will be used.
#' @param addon Character or character vector. One or more addons. See addons() for options.
# @param backup Logical. If TRUE, a backup copy is made when an existing library is opened.
# @param backupName Character. Added to a library filepath to create a backup library.
# @param backupOverwrite Logical. If TRUE, the existing backup of a library (if any) will be overwritten.
#' @param forceUpdate Logical. If FALSE (default) user will be prompted to approve any required updates. If TRUE, required updates will be applied silently.
#' @return An \code{SsimLibrary} object representing a SyncroSim library.
#' @examples
#' #TODO – update examples
#' # See the installed models
#' models(session())
#'
#' # Create a library called <model>.ssim in the current working directory.
#' myLibrary = ssimLibrary()
#' session(myLibrary) #The SycroSim session
#' filepath(myLibrary) #Path to the file on disk.
#' info(myLibrary) #Model type and other library information.
#'
#' # Open an existing SyncroSim library in the current working directory - don't make a backup copy.
#' myLibrary = ssimLibrary()
#'
#' # Create a library with a name in the current working directory
#' mySecondLibrary = ssimLibrary(name="Lib2")
#'
#' # Create a library with a name in another directory
#' myThirdLibrary = ssimLibrary(name=paste0(getwd(),"/Temp/Lib3"))
#'
#' # Create or load a library using a specific session
#' mySession = session("C:/Program Files/SyncroSim/1/SyncroSim.Console.exe")
#' myLibrary = ssimLibrary(name="Lib2",session=mySession)
#'
#' # Add a project and get the library associated with that project
#' myProject = project(myLibrary,project="a project")
#' myLibrary = ssimLibrary(myProject)
#' @name ssimLibrary
setMethod('ssimLibrary',signature(name="missingOrNULLOrChar"),
          function(name=NULL,model=NULL,session=NULL,addon=NULL,forceUpdate=F) {
    new("SsimLibrary",name,model,session,addon,forceUpdate,create=T)
})

#' @describeIn ssimLibrary Get the SsimLibrary associated with a SyncroSim Object.
setMethod('ssimLibrary', signature(name="SsimLibrary"), function(name) {
  #model=cScn
  if(class(name)=="SsimLibrary"){
    out=name
  }else{
    out = .ssimLibrary(name=.filepath(name),session=.session(name),create=F)
  }
  return(out)
})

setMethod('filepath', signature(x="SsimLibrary"), function(x) x@filepath)

setMethod('info', signature(x="SsimLibrary"), function(x) {
  #x=myLibrary
  args = list(list=NULL,library=NULL,csv=NULL,lib=.filepath(x))
  tt = command(args,.session(x))
  out = .dataframeFromSSim(tt,localNames=T)
  return(out)
})

setMethod('name', signature(ssimObject="SsimLibrary"), function(ssimObject) {
  #ssimObject=myLibrary
  cInfo = info(ssimObject)
  return(subset(cInfo,property=="Name:")$value)
})

setMethod('dateModified', signature(ssimObject="SsimLibrary"), function(ssimObject) {
  #ssimObject=myLibrary
  cInfo = info(ssimObject)
  return(subset(cInfo,property=="Last Modified:")$value)
})

setMethod('owner', signature(ssimObject="SsimLibrary"), function(ssimObject) {
  #ssimObject=myLibrary
  cInfo = info(ssimObject)
  return(subset(cInfo,property=="Owner:")$value)
})

setMethod('readOnly', signature(ssimObject="SsimLibrary"), function(ssimObject) {
  #ssimObject=myLibrary
  cInfo = info(ssimObject)
  return(subset(cInfo,property=="Read Only:")$value)
})
setReplaceMethod(
  f='name',
  signature="SsimLibrary",
  definition=function(ssimObject,value){
    #x=myScenario;value="New Name"
    tt = command(list(setprop=NULL,lib=.filepath(ssimObject),name=value),.session(ssimObject))
    if(!identical(tt,"saved")){
      stop(tt)
    }
    return (ssimObject)
  }
)


#' The name of the primary model associate with a SyncroSim object
#'
#' The name of the primary model associated with a SSimLibarary, Project or Scenario.
#'
#' @param x An object with an associated primary model.
#' @return A model name
#' @export
setGeneric('modelName',function(x) standardGeneric('modelName'))
setMethod('modelName', signature(x="SsimLibrary"), function(x) {
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
setMethod('modelVersion', signature(x="SsimLibrary"), function(x) {
  #x = myLibrary
  cInfo = info(x)
  out=paste(cInfo$property[cInfo$property=="Source Module Version:"],cInfo$value[cInfo$property=="Source Module Version:"])
  return(out)
})

#' Apply updates.
#'
#' Apply updates to a SyncroSim Library.
#'
#' @param x An SsimLibrary object, or a Project or Scenario associated with a Library
#' @return "saved" or a failure message from the console.
#' @export
setGeneric('update',function(x) standardGeneric('update'))
setMethod('update', signature(x="SsimLibrary"), function(x) {
  #x= myLibrary
  #args = list(update=NULL,lib=.filepath(x));session=.session(x)
  tt = command(list(update=NULL,lib=.filepath(x)),.session(x))
  return(tt[1])
})

#' Delete library, project, scenario, datasheet, or list of these
#'
#' Deletes one or more items. Note this is irreversable.
#'
#' @param ssimObject SsimLibrary/Project/Scenario, path to a library, or list of these. Note that project/scenario arguments are ignored if ssimObject is a list.
#' @param project character, numeric, or vector of these. One or more project names or ids. Note that project argument is ignored if ssimObject is a list.
#' @param scenario character, numeric, or vector of these. One or more project names or ids. Note that scenario argument is ignored if ssimObject is a list.
#' @param datasheet character, numeric, or vector of these. One or more project names or ids.
#' @param force logical. If FALSE (default), user will be prompted to approve removal of each item.
#' @return A list of "saved" or failure messages for each item.
#' @examples
#' TODO – update examples
#' myLibrary = ssimLibrary(session=devSession)
#' myProject = project(myLibrary,project="a project")
#' project(myLibrary)
#' removeProject(myLibrary,project="a project")
#' project(myLibrary)
#'
#' @export
setGeneric('delete',function(ssimObject,project=NULL,scenario=NULL,datasheet=NULL,force=F) standardGeneric('delete'))
setMethod('delete', signature(ssimObject="character"), function(ssimObject,project,scenario,datasheet,force) {
  ssimObject=.ssimLibrary(ssimObject,create=F)
  return(delete(ssimObject,project,scenario,datasheet,force))
})
setMethod('delete', signature(ssimObject="list"), function(ssimObject,project,scenario,datasheet,force) {
  x = getIdsFromListOfObjects(ssimObject,project=project)
  ssimObject = x$ssimObject
  expecting=x$expecting
  if(expecting=="Project"){
    return(delete(ssimObject,project=x$objs,scenario=NULL,datasheet=datasheet,force=force))
  }
  if(expecting=="Scenario"){
    return(delete(ssimObject,project=NULL,scenario=x$objs,datasheet=datasheet,force=force))
  }
  
  if(expecting=="SsimLibrary"){
    out = list()
    for(i in seq(length.out=length(x$objs))){
      if(is.null(datasheet)){
        cObj = x$objs[i]
        out[[.filepath(cObj)]]=deleteLibrary(cObj,force)
      }else{
        out[[.filepath(cObj)]]=delete(cObj,project=NULL,scenario=NULL,datasheet=datasheet,force=force)
      }
    }
    return(out)
  }
  stop("Problem with ssimObject: should be a list of SsimLibraries/Projects/Scenarios or paths to libraries.")
})

setMethod('delete', signature(ssimObject="SsimLibrary"), function(ssimObject,project,scenario,datasheet,force) {
  #ssimObject = myLibrary; project=.projectId(myProject);datasheet="STSim_StateLabelX";force=F
  xProjScn=.getFromXProjScn(ssimObject,project=project,scenario=scenario,returnIds=T,convertObject=F,complainIfMissing=T)
  
  #expect to have a vector of valid project ids - checking already done
  x=xProjScn$ssimObject
  project=xProjScn$project
  scenario=xProjScn$scenario
  goal=xProjScn$goal
  
  if(goal=="library"){
    if(is.null(datasheet)){
      out=deleteLibrary(ssimObject,force)
    }else{
      datasheets=.datasheets(ssimObject)
      out = deleteDatasheet(datasheet,datasheets,cProj=NULL,cScn=NULL,cProjName=NULL,cScnName=NULL,force=force)
    }
    return(out)
  }
  
  if(goal=="project"){
    allProjects = xProjScn$projectSet
    
    if(!is.numeric(project)){
      stop("Error in delete: expect to have valid project ids.")
    }
    
    if(!is.null(datasheet)){
      if(is.element(class(ssimObject),c("Project","Scenario"))){
        datasheets=.datasheets(ssimObject,refresh=T)
      }else{
        datasheets=.datasheets(.project(ssimObject,project = project[1]))
      }
    }
    
    out = list()
    for(i in seq(length.out=length(project))){
      #i = 1
      cProj = project[i]
      name=allProjects$name[allProjects$id==cProj]
      
      #If datasheets(s) specified delete them. Otherwise delete the projects.
      if(!is.null(datasheet)){
        out = deleteDatasheet(datasheet,datasheets,cProj=cProj,cScn=NULL,cProjName=name,cScnName=NULL,out=out,force=force)
      }else{
        if(force){
          answer="y"
        }else{
          answer <- readline(prompt=paste0("Do you really want to delete project ",name,"(",cProj,")? (y/n): "))
        }
        if(answer=="y"){
          outBit = command(list(delete=NULL,project=NULL,lib=.filepath(x),pid=cProj,force=NULL),.session(x))
        }else{
          outBit = "skipped"
        }
        
      }
      out[[as.character(cProj)]]=outBit
    }
    return(out)
  }
  
  if(goal=="scenario"){
    allScenarios = xProjScn$scenarioSet
    
    if(!is.numeric(scenario)){
      stop("Error in delete: expect to have valid scenario ids.")
    }
    
    if(!is.null(datasheet)){
      if(is.element(class(ssimObject),c("Scenario"))){
        datasheets=.datasheets(ssimObject,refresh=T)
        scenarioSet = scenario(.ssimLibrary(ssimObject),summary=T)
      }else{
        datasheets=.datasheets(.scenario(ssimObject,scenario = scenario[1]))
        scenarioSet = scenario(ssimObject,summary=T)
      }
    }
    out = list()
    for(i in seq(length.out=length(scenario))){
      #i = 1
      cScn = scenario[i]
      name = allScenarios$name[allScenarios$id==cScn]
      if(!is.null(datasheet)){
        cProj = subset(scenarioSet,id==cScn)$pid
        out = deleteDatasheet(datasheet,datasheets,cProj=cProj,cScn=cScn,cProjName="",cScnName=name,out=out,force=force)
      }else{
        if(force){
          answer="y"
        }else{
          answer <- readline(prompt=paste0("Do you really want to remove scenario ",name,"(",cScn,")? (y/n): "))
        }
        if(answer=="y"){
          outBit = command(list(delete=NULL,scenario=NULL,lib=.filepath(x),sid=cScn,force=NULL),.session(x))
        }else{
          outBit = "skipped"
        }
        out[[as.character(cScn)]]=outBit
      }
    }
    return(out)
  }
  stop("Error in delete().")
})

# Delete Library
#
# Deletes a SyncroSim library. Note this is irreversable.
#
# @param ssimLibrary SsimLibrary or path to a library
# @param force Logical. If FALSE (default) prompt to confirm that the library should be deleted. This is irreversable.
# @return "saved" or failure message.
# @examples
#
# @export
setGeneric('deleteLibrary',function(ssimLibrary,force=F) standardGeneric('deleteLibrary'))
setMethod('deleteLibrary', signature(ssimLibrary="SsimLibrary"), function(ssimLibrary,force) {
  #ssimLibrary = .ssimLibrary(name="temp26",session=mySession,create=T)
  if(!file.exists(.filepath(ssimLibrary))){
    return(paste0("Library not found: ",.filepath(ssimLibrary)))
  }
  if(force){
    answer="y"
  }else{
    answer <- readline(prompt=paste0("Do you really want to delete library ",.filepath(ssimLibrary),"? (y/n): "))
  }
  if(answer=="y"){
    #ssimLibrary=myLibrary;.filepath(myLibrary)
    unlink(.filepath(ssimLibrary))
    unlink(paste0(.filepath(ssimLibrary),".backup"),recursive=T,force=T)
    unlink(paste0(.filepath(ssimLibrary),".input"),recursive=T,force=T)
    unlink(paste0(.filepath(ssimLibrary),".output"),recursive=T,force=T)
    return("saved")
  }else{
    return("skipped")
  }
})

#' addons of an SsimLibrary or Session
#'
#' The addons of an SsimLibrary or Session.
#'
#' @param ssimObject SsimLibrary/Project/Scenario or Session.
#' @param all If TRUE, all available addons are returned. Otherwise, only enabled addons.
#' @return A dataframe of addons.
#' @examples
#' addons(ssimLibrary(name="stsim"))
#' @export
setGeneric('addons',function(ssimObject,all=F) standardGeneric('addons'))
setMethod('addons', signature(ssimObject="Session"), function(ssimObject,all) {
  #x = myLibrary
  tt = command(list(list=NULL,addons=NULL,csv=NULL),ssimObject)
  tt = .dataframeFromSSim(tt)
  tt$shortName = gsub(":add-on-transformer","",tt$name,fixed=T)
  return(tt)
})

setMethod('addons', signature(ssimObject="SsimLibrary"), function(ssimObject,all) {
  #x = myLibrary
  tt = command(list(list=NULL,addons=NULL,csv=NULL,lib=.filepath(ssimObject)),.session(ssimObject))
  tt = .dataframeFromSSim(tt)
  if(!all){
    tt=subset(tt,enabled=="Yes")
  }
  tt$shortName = gsub(":add-on-transformer","",tt$name,fixed=T)
  return(tt)
})

#' Enable addons.
#'
#' Enable addons of an SsimLibrary, or Project/Scenario with an associated SsimLibrary.
#'
#' @param x= A SsimLibrary, Project or Scenario.
#' @return x
#' @examples
#' myLibrary = ssimLibrary()
#' enableAddons(myLibrary)=c("stsim-ecological-departure", "stsim-stock-flow")
#' addons(myLibrary)
#' @export
setGeneric('enableAddons<-',function(x,value) standardGeneric('enableAddons<-'))
setReplaceMethod(
  f='enableAddons',
  signature="SsimLibrary",
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
      if(!identical(tt,"saved")){print(paste(tt[1],cVal))}
    }

    x@datasheetNames = .datasheets(x,scope="all",refresh=T)
    return (x)
  }
)

#' Disable addons.
#'
#' Disable addons an SsimLibrary, or Project/Scenario with an associated SsimLibrary.
#'
#' @param x= A SsimLibrary, Project or Scenario.
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
  signature="SsimLibrary",
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
      if(!identical(tt,"saved")){print(paste(tt[1],cVal))}
    }

    x@datasheetNames = .datasheets(x,scope="all",refresh=T)
    return (x)
  }
)

setMethod('datasheets', signature(x="SsimLibrary"), function(x,project,scenario,scope,refresh) {
  #x = myScn;project=NULL;scenario=NULL;empty=T;scope=NULL;refresh=T
  x = .getFromXProjScn(x,project,scenario)

  #command(c("list","datasheets","help"),.session(myLibrary))
  #Get datasheet dataframe
  if(!refresh){
    datasheets=x@datasheetNames
  }else{
    #x=myLibrary
    tt=command(c("list","datasheets","csv",paste0("lib=",.filepath(x))),.session(x))
    if(grepl("The library has unapplied updates",tt[[1]])){
      stop(tt)
    }
    datasheets = .dataframeFromSSim(tt,convertToLogical=c("isOutput","isSingle"))
    datasheets$scope = sapply(datasheets$scope,camel)
    #names(datasheets) = c("name","displayName","dataScope","isOutput")
    #datasheets$isSpatial = grepl("Spatial",datasheets$name)&!grepl("NonSpatial",datasheets$name)
    #TO DO - export this info from SyncroSim
  }
  datasheets$order=seq(1,nrow(datasheets))
  if(!is.null(scope)&&(scope=="all")){
    datasheets$order=NULL
    return(datasheets)
  }
  if(is.element(class(x),c("Project","SsimLibrary"))){
    datasheets = subset(datasheets,scope!="scenario")
  }
  if(is.element(class(x),c("SsimLibrary"))){
    datasheets = subset(datasheets,scope!="project")
  }
  if(!is.null(scope)){
    if(!is.element(scope,c("scenario","project","library"))){
      stop("Invalid scope ",scope,". Valid scopes are 'scenario', 'project', 'library' and NULL.")
    }
    cScope=scope
    datasheets=subset(datasheets,scope==cScope)
  }
  datasheets=datasheets[order(datasheets$order),];datasheets$order=NULL
  return(datasheets)
})

setMethod('datasheet', signature(ssimObject="SsimLibrary"), function(ssimObject,name,project,scenario,summary,optional,empty,lookupsAsFactors,sqlStatements,includeKey,forceElements) {
  #ssimObject = myResult[[1]];name="STSim_OutputSpatialState";project=NULL;scenario=NULL;summary=NULL;optional=F
  #empty=F;lookupsAsFactors=T;sqlStatements=list(select="SELECT *",groupBy="");includeKey=F;forceElements=F

  xProjScn = .getFromXProjScn(ssimObject,project,scenario,returnIds=T,convertObject=F,complainIfMissing=T)
  if(class(xProjScn)=="SsimLibrary"){
    x=xProjScn
    pid=NULL
    sid=NULL
  }else{
    x = xProjScn$ssimObject
    pid=xProjScn$project
    sid=xProjScn$scenario
  }
  #now have valid pid/sid vectors and x is library.

  allNames=name
  
  if(is.null(summary)){
    if(is.null(name)){summary=T}else{summary=F}
  }
  
  #if summary, don't need to bother with project/scenario ids: sheet info doesn't vary among project/scenarios in a project
  if(summary|is.null(name)){
    sumInfo = .datasheets(x,project[[1]],scenario[[1]])
    sumInfo$order=seq(1,nrow(sumInfo))
    if(is.null(name)){
      name=sumInfo$name
      allNames=name
    }
    missingSheets = setdiff(name,sumInfo$name)
    if(length(missingSheets)>0){
      sumInfo = .datasheets(x,project[[1]],scenario[[1]],refresh=T)
      missingSheets = setdiff(name,sumInfo$name)
      if(length(missingSheets)>0){
        stop(paste0("Datasheets not found: ",paste(missingSheets,collapse=",")))
      }
    }
    sumInfo=subset(sumInfo,is.element(name,allNames))
  }

  #now assume we have one or more names 
  if(is.null(name)){stop("Something is wrong in datasheet().")}
  #if(summary){stop("Something is wrong in datasheet().")}
  
  if(summary&!optional){
    sumInfo=subset(sumInfo,select=c("scope","name","displayName","order"))
    sumInfo[order(sumInfo$order),];sumInfo$order=NULL;return(sumInfo)
  }

  #Add hasData info - only for scenario scope datasheets if sid is defined
  if(summary){
    #if no scenario scope sheets, return sumInfo without checking for hasData
    scnSheetSum = sum(sumInfo$scope=="scenario")
    
    if(scnSheetSum==0){sumInfo[order(sumInfo$order),];sumInfo$order=NULL;return(sumInfo)}
    for(i in seq(length.out=length(sid))){
      #i=1
      cSid = sid[i]
      tt = command(list(list=NULL,datasources=NULL,lib=.filepath(x),sid=cSid),session=session(x))

      if(grepl("The library has unapplied updates",tt[[1]])){
        stop(tt)
      }
      hasDataInfo = .dataframeFromSSim(tt,csv=F,convertToLogical=c("hasData","dataInherited"))
      
      if(length(setdiff(hasDataInfo$name,sumInfo$name))>0){
        sumInfo = .datasheets(x,project[[1]],scenario[[1]],refresh=T)
        sumInfo$order=seq(1,nrow(sumInfo))
        if(is.null(name)){
          name=sumInfo$name
          allNames=name
        }
      }
      if(sum(hasDataInfo$dataInherited)>0){
        addCols = c("name","hasData","dataInherited","dataSource")
      }else{
        addCols=c("name","hasData")
      }
      hasDatBit = subset(hasDataInfo,select=addCols)
      hasDatBit$scenario = i

      if(i ==1){
        hasDatAll = hasDatBit
      }else{hasDatAll = rbind(hasDatAll,hasDatBit)}
    }
      
    prevNames=names(sumInfo)
    sumInfo=merge(sumInfo,hasDatAll,all.x=T)
    sumInfo=subset(sumInfo,select=c(prevNames,setdiff(names(sumInfo),prevNames)))
    sumInfo=sumInfo[order(sumInfo$order,sumInfo$scenario),];sumInfo$order=NULL
    return(sumInfo)
  }

  dir.create(paste0(dirname(.filepath(x)),"/Temp"), showWarnings = FALSE)

  outSheetList = list()
  for(kk in seq(length.out=length(allNames))){
    name=allNames[kk]
    
    if(!includeKey){
      rmId = strsplit(name,"_")[[1]][2]
      rmCols = c(paste0(rmId,"ID"))
    }else{
      rmCols=c()
    }
    
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
      useConsole = useConsole&!((sheetNames$scope=="project")&(length(pid)>1))
      useConsole = useConsole&!((sheetNames$scope=="scenario")&(length(sid)>1))
      
      if(useConsole){
        unlink(tempFile)
        if(!optional){
          args =list(export=NULL,lib=.filepath(x),sheet=name,file=tempFile,valsheets=NULL,force=NULL,includepk=NULL,colswithdata=NULL,extfilepaths=NULL)#filepath=NULL
        }else{
          args =list(export=NULL,lib=.filepath(x),sheet=name,file=tempFile,valsheets=NULL,force=NULL,includepk=NULL,extfilepaths=NULL)#filepath=NULL
        }
        if(sheetNames$scope=="project"){args[["pid"]]=pid}
        
        if(is.element(sheetNames$scope,c("project","scenario"))){args[["pid"]]=pid}
        if(sheetNames$scope=="scenario"){args[["sid"]]=sid}
        
        tt=command(args,.session(x))
        
        if(!identical(tt,"saved")){
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
        if(is.null(sqlStatements$where)){sqlStatements$where = ""}
        sqlStatements$from = paste("FROM",name)
        if(sheetNames$scope=="scenario"){
          if(is.null(sid)){
            stop("Specify a scenario.")
          }else{
            #following http://faculty.washington.edu/kenrice/sisg-adv/sisg-09.pdf
            #and http://www.sqlitetutorial.net/sqlite-in/
            if(sqlStatements$where==""){
              sqlStatements$where = paste0("WHERE ScenarioID IN (",paste(sid,collapse=","),")")
            }else{
              sqlStatements$where = paste0(sqlStatements$where," AND (ScenarioID IN (",paste(sid,collapse=","),"))")
            }
          }
        }
        if(sheetNames$scope=="project"){
          if(is.null(pid)){
            stop("Specify a project.")
          }else{
            if(sqlStatements$where==""){
              sqlStatements$where = paste0("WHERE ProjectID IN (",paste(pid,collapse=","),")")
            }else{
              sqlStatements$where = paste0(sqlStatements$where," AND (ProjectID IN (",paste(pid,collapse=","),"))")
            }
          }
        }
        #  sheet = DBI::dbReadTable(con, name)
        sql = paste(sqlStatements$select,sqlStatements$from,sqlStatements$where,sqlStatements$groupBy)
        #sql = paste("SELECT ScenarioID,Iteration,Timestep,StratumID,SecondaryStratumID,StateClassID,StateLabelXID,StateLabelYID, SUM(Amount)",sqlStatements$from,sqlStatements$where,sqlStatements$groupBy)
        # print(sql)
        sheet = DBI::dbGetQuery(con,sql)
        DBI::dbDisconnect(con)
        
        #Filter out columns without data
        if(!optional&&(nrow(sheet)>0)){
          colNames = names(sheet)
          for(r in seq(length.out=length(colNames))){
            cCol = colNames[r]
            if(sum(!is.na(sheet[[cCol]]))==0){
              sheet[[cCol]]=NULL
            }
          }
        }
      }
    }else{
      sheet=data.frame(temp=NA)
      sheet=subset(sheet,!is.na(temp))
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
          sheetInfo$optional[is.element(sheetInfo$name,names(sheet))&(sheetInfo$optional=="Yes")]="Present"
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
          args =list(export=NULL,lib=.filepath(x),sheet=name,file=tempFile,valsheetsonly=NULL,force=NULL,includepk=NULL)
          if(sheetNames$scope=="project"){args[["pid"]]=pid}
          if(is.element(sheetNames$scope,c("project","scenario"))){args[["pid"]]=pid}
          if(sheetNames$scope=="scenario"){args[["sid"]]=sid}
          tt=command(args,.session(x))
          if(!identical(tt,"saved")){
            stop(tt)
          }
        }
      }
      for(i in seq(length.out=nrow(sheetInfo))){
        #i =5
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
          cIDs = c()
          for(j in seq(length.out=length(opts))){
            cLevels=c(cLevels,strsplit(opts[j],":",fixed=T)[[1]][2])
            cIDs = as.numeric(c(cIDs,strsplit(opts[j],":",fixed=T)[[1]][1]))
          }
          #Sometimes input is factors, and output is  IDs
          if(length(setdiff(sheet[[cRow$name]],cIDs))==0){
            warning(paste0("Converting ",cRow$name," IDs to factor levels"))
            mergeBit=data.frame(oLev = cLevels)
            mergeBit[[cRow$name]]=cIDs
            sheet=merge(sheet,mergeBit,all.x=T)
            sheet[[cRow$name]]=sheet$oLev;sheet$oLev=NULL
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
                  allScns = scenario(x)
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
            if(nrow(lookupSheet)>0){
              lookupSheet=lookupSheet[order(lookupSheet[[names(lookupSheet[1])]]),]
              lookupLevels = lookupSheet$Name
            }else{
              lookupLevels = c()
            }
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
          if(cRow$valCond=="Between"){
            print(paste0("Note: ",cRow$name," should be between ",cRow$formula1," and ",cRow$formula2))
            
          }else{
            stop("handle this case")
          }
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
            allProjects = .project(x)
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
          
          allScns = scenario(x)
          if(!is.element("parentId",names(allScns))){
            warning("Missing parentId info from scenario(summary=T).")
            allScns$parentId=NA
          }
          allScns=subset(allScns,select=c(id,pid,name,parentId))
          names(allScns) = c("ScenarioID","ProjectID","ScenarioName","ScenarioParent")
          sheet=merge(allScns,sheet,all.Y=T)
        }
      }
    }
    outSheetList[[cName]]=sheet
    
    #return single row datasheets as named vectors (if not for multiple scenarios)
    #note info about data types and lookups will be lost if we do this. so don't.
    if(FALSE&&sheetNames$isSingle&&(nrow(sheet)<=1)){
      if(nrow(sheet)==0){
        vec = rep(" ",ncol(sheet))
        vec=
        names(vec)=names(sheet)
        
      }else{
        vec = unlist(sheet[1,])
      }
    }
    
  }  
  
  if(!forceElements&(length(outSheetList)==1)){
    outSheetList=outSheetList[[1]]  
  }
  
  return(outSheetList)
})

setMethod('saveDatasheet', signature(ssimObject="SsimLibrary"), function(ssimObject,data,name,project,scenario,append,forceElements) {
  #ssimObject = myProject;project=NULL;scenario=NULL;name=sheetName;data=mySheet;append=NULL;forceElements=F
  x = .getFromXProjScn(ssimObject,project,scenario,convertObject=T,returnIds=F)
  if(class(x)=="list"){
    stop("ssimObject/project/scenario should uniquely identify a single ssimObject.")
  }
  if(is.null(append)){
    if(class(x)=="Scenario"){append=F}else{append=T}
  }
  
  sheetNames = .datasheets(x)

  #Note - cannot handle a list of named vectors, only a list of dataframes.
  if((class(data)!="list")|(class(data[[1]])!="data.frame")){
    if(is.null(name)){
      stop("Need a datasheet name.")
    }
    if(length(name)>1){
      stop("If a vector of names is provided, then data must be a list.")
    }
    hdat = data
    data = list()
    data[[name]]=hdat
  }else{
    if(!is.null(name)){
      if(length(name)!=length(data)){
        stop("Please provide a name for each element of data.")
      }  
      warning("name argument will override names(data).")
      names(data)=name
    }else{
      name=names(data)    
    }
  }

  out=list()
  for(i in seq(length.out=length(data))){
    #i=1
    cName = names(data)[i]
    cDat = data[[cName]]
    
    #handle cases when cDat is not a data.frame
    #cDat = "Coniferous";names(cDat)="Name"
    if(class(cDat)!="data.frame"){
      cIn= cDat
      if(length(cIn)==0){
        stop("No data found for ",cName)
      }
      if(!is.null(names(cDat))){
        cDat=data.frame(a=cIn[[1]])
        names(cDat)=names(cIn)[1]
        for(j in seq(length.out=(length(cIn)-1))){
          cDat[[names(cIn)[j+1]]]=cIn[[j+1]]
        }
      }else{
        stop("handle this case")
      }
    }
    
    for(j in seq(length.out=ncol(cDat))){
      if(is.factor(cDat[[j]])){cDat[[j]]=as.character(cDat[[j]])}
      if(is.logical(cDat[[j]])){
        inCol = cDat[[j]]
        cDat[[j]][inCol]="Yes";cDat[[j]][!inCol]="No"
      }
    }
    cDat[is.na(cDat)]=""

    if(FALSE&&breakpoint){pathBit = paste0(.filepath(x),'.temp/Data')}else{pathBit = paste0(dirname(.filepath(x)),'/Temp')}

    dir.create(pathBit, showWarnings = FALSE,recursive=T)
    tempFile = paste0(pathBit,"/",cName,".csv")

    write.csv(cDat,file=tempFile,row.names=F,quote=T)
    if(FALSE&&breakpoint){
      out[[cName]] = "Saved"
      next
    }

    args = list(import=NULL,lib=.filepath(x),sheet=cName,file=tempFile)
    scope =sheetNames$scope[sheetNames$name==cName]
    if(length(scope)==0){
      sheetNames = datasheets(x,refresh=T)
      scope =sheetNames$scope[sheetNames$name==cName]
      if(length(scope)==0){
        stop("name not found in datasheetNames")
      }
    }
    doDelete = F
    if(scope=="scenario"){
      if(append){args[["append"]]=NULL}else{
        if(nrow(cDat)==0){
          doDelete=T
        }
      }
    }else{
      if(!append){
        doDelete=T
      }
    }
    if(doDelete){
      targs = list(delete=NULL,data=NULL,lib=.filepath(x),sheet=cName,force=NULL)
      if(scope=="scenario"){
        targs[["sid"]]=.scenarioId(x)
      }
      if(scope=="project"){
        targs[["pid"]]=.projectId(x)
      }
      ttt=command(targs,.session(x))
      if(ttt[[1]]!="saved"){
        stop(ttt)
      }
    }

    tt="saved"
    if(nrow(cDat)>0){
      if(scope=="project"){args[["pid"]]=.projectId(x)}
      if(scope=="scenario"){args[["sid"]]=.scenarioId(x)}
      tt=command(args,.session(x))
    }
    if(tt[[1]]=="saved"){unlink(tempFile)}
    out[[cName]] = tt
  }
  
  if(!forceElements&&(length(out)==1)){
    out=out[[1]]
  }
  return(out)
})

setMethod('run', signature(ssimObject="SsimLibrary"), function(ssimObject,scenario,summary,jobs,forceElements) {
  #x=myScenario;jobs=2;scenario=NULL
  
  xProjScn = .getFromXProjScn(ssimObject,scenario=scenario,convertObject=F,returnIds=T,goal="scenario",complainIfMissing=T)
  #Now assume scenario is x is valid object and scenario is valid vector of scenario ids
  x = xProjScn$ssimObject
  scenario = xProjScn$scenario
  scenarios = xProjScn$scenarioSet
  
  if(!is.numeric(scenario)){stop("Error in run(): expecting valid scenario ids.")}
  
  #name(x)
  scenarios=NULL
  out=list()
  for(i in seq(length.out=length(scenario))){
    #i =1
    cScn = scenario[i]
    name = scenarios$name[scenarios$id==cScn]


    print(paste0("Running scenario ",name," (",cScn,")..."))

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
      if(is.na(resultId)){
        stop(print(tt))
      }
      
    }else{
      #x=myFlatScenario;jobs=2
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
      resultId =   run(cBreakpointSession,jobs=jobs)
      resp = writeLines("shutdown", connection(cBreakpointSession),sep = "")
      close(connection(cBreakpointSession)) # Close the connection.
      cBreakpointSession=NULL
    }
    #Resume here.
    #stop
    inScn = paste0(name," (",cScn,")")
    if(!identical(resultId,suppressWarnings(as.character(as.numeric(resultId))))){
      out[[inScn]]=tt
      print(tt)
    }else{
      if(summary){
        out[[inScn]] = as.numeric(resultId)
        multiband(.scenario(x,scenario=as.numeric(resultId)),action="apply")
      }else{
        out[[inScn]] = .scenario(x,scenario=as.numeric(resultId))
        multiband(out[[inScn]],action="apply")
      }
    }

  }
  
  if(!forceElements&&(class(out)=="list")&&(length(out)==1)){
    out=out[[1]]
  }
  
  if(summary&&(class(out)=="list")){
    #output named vector of ids
    out = list("1"=2,"3"=4)
    out=unlist(out)
  }
  return(out)
})
