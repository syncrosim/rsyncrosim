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
      modelOption = models(session)
      model=gsub(":model-transformer","",model,fixed=T)
      if(!is.element(model,modelOptions$name)){
        stop(paste("Model type",model,"not recognized. Options are:",paste0(modelOptions$name,collapse=",")))
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
        modelOptions = model(session)
      }

      args = list(create=NULL,library=NULL,name=path,model=modelOptions$name[modelOptions$name==model])
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
      
      if(!exists("modelOptions")){modelOptions=model(session)}
      expectedModule = modelOptions$name[modelOptions$name==model]
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
#' model(session())
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
  oVal  = subset(cInfo,property=="Read Only:")$value
  rVal=oVal
  if(oVal=="Yes"){rVal=T}
  if(oVal=="No"){rVal=F}
  return(rVal)
})

setMethod('description', signature(ssimObject="SsimLibrary"), function(ssimObject) {
  #ssimObject=myLibrary
  desc = command(list(list=NULL,description=NULL,lib=.filepath(ssimObject)),session=.session(ssimObject))
  return(desc)
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

setReplaceMethod(
  f='description',
  signature="SsimLibrary",
  definition=function(ssimObject,value){
    #x=myScenario;value="New description"
    inValue = value
    if(length(inValue)>1){
      value=""
      for(i in 1:length(inValue)){
        value=paste0(value,inValue[[i]],sep="\n")
      }
    }
    value = gsub("\n","\\n",value,fixed=T)
    #value=paste0('\"',value,'\"')
    args = list(setprop=NULL,lib=.filepath(ssimObject),description=value)
    if(class(ssimObject)=="Project"){args$pid = .projectId(ssimObject)}
    if(class(ssimObject)=="Scenario"){args$sid = .scenarioId(ssimObject)}
    tt = command(args,.session(ssimObject))
    if(!identical(tt,"saved")){
      stop(tt)
    }
    return (ssimObject)
  }
)

setReplaceMethod(
  f='owner',
  signature="SsimLibrary",
  definition=function(ssimObject,value){
    #x=myScenario;value="New description"
    args = list(setprop=NULL,lib=.filepath(ssimObject),owner=value)
    if(class(ssimObject)=="Project"){args$pid = .projectId(ssimObject)}
    if(class(ssimObject)=="Scenario"){args$sid = .scenarioId(ssimObject)}
    tt = command(args,.session(ssimObject))
    if(!identical(tt,"saved")){
      stop(tt)
    }
    return (ssimObject)
  }
)

setReplaceMethod(
  f='readOnly',
  signature="SsimLibrary",
  definition=function(ssimObject,value){
    #value = F;ssimObject=myLibrary
    if(class(value)!="logical"){
      stop("readOnly must be TRUE or FALSE.")
    }
    if(value==T){readOnly = "yes"}else{readOnly="no"}
    args = list(setprop=NULL,lib=.filepath(ssimObject),readonly=readOnly)
    if(class(ssimObject)=="Project"){args$pid = .projectId(ssimObject)}
    if(class(ssimObject)=="Scenario"){args$sid = .scenarioId(ssimObject)}
    tt = command(args,.session(ssimObject))
    if(!identical(tt,"saved")){
      stop(tt)
    }
    return (ssimObject)
  }
)

#' Apply updates.
#'
#' Apply updates to a SyncroSim Library.
#'
#' @param x An SsimLibrary object, or a Project or Scenario associated with a Library
#' @return "saved" or a failure message from the console.
#' @export
setGeneric('ssimUpdate',function(x) standardGeneric('ssimUpdate'))
setMethod('ssimUpdate', signature(x="SsimLibrary"), function(x) {
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


#' Enable addon or addons.
#'
#' Enable addon or addons of an SsimLibrary.
#'
#' @param ssimLibrary SsimLibrary
#' @param name Character string or vector of these.
#' @return saved or error message for each addon.
#' @examples
#' myLibrary = ssimLibrary()
#' enableAddon(myLibrary,c("stsim-ecological-departure", "stsim-stock-flow"))
#' addons(myLibrary)
#' @export
setGeneric('enableAddon',function(ssimLibrary,name) standardGeneric('enableAddon'))
setMethod('enableAddon', signature(ssimLibrary="SsimLibrary"), function(ssimLibrary,name) {
    cAdds = addons(ssimLibrary,all=T)
    name=gsub(":add-on-transformer","",name,fixed=T)
    retList=list()
    for(i in seq(length.out=length(name))){
      #i=1
      cVal = name[i]
      if(!is.element(cVal,cAdds$shortName)){
        print(paste0("Warning - ",cVal," is not among the available addons: ",paste(cAdds$shortName[cAdds$enabled=="No"],collapse=",")))
        next
      }
      cAddsLess = subset(cAdds,enabled=="No")
      if(!is.element(cVal,cAddsLess$shortName)){
        print(paste0(cVal," is already enabled."))
        next
      }

      tt=command(list(create=NULL,addon=NULL,lib=.filepath(ssimLibrary),name=paste0(cVal,":add-on-transformer")),.session(ssimLibrary))
      retList[[cVal]]=tt
    }

    return (retList)
  }
)

#' Disable addon or addons.
#'
#' Disable addon or addons of an SsimLibrary, or Project/Scenario with an associated SsimLibrary.
#'
#' @param ssimLibrary SsimLibrary
#' @param name Character string or vector of these.
#' @return saved or error message.
#' @examples
#' TODO - update examples
#' myLibrary = ssimLibrary()
#' enableAddon(myLibrary,c("stsim-ecological-departure"))
#' addons(myLibrary)
#' disableAddon(myLibrary,c("stsim-ecological-departure"))
#' addons(myLibrary)
#'
#' @export
setGeneric('disableAddon',function(ssimLibrary,name) standardGeneric('disableAddon'))
setMethod('disableAddon', signature(ssimLibrary="SsimLibrary"), function(ssimLibrary,name) {
    #x=myLibrary
    #value = c("stsim-ecological-departure", "stsim-stock-flow")
    cAdds = addons(ssimLibrary,all=T)
    name=gsub(":add-on-transformer","",name,fixed=T)
    retList = list()
    for(i in seq(length.out=length(name))){
      #i=1
      cVal = name[i]
      if(!is.element(cVal,cAdds$shortName)){
        print(paste0("Warning - ",cVal," is not among the available addons: ",paste(cAdds$shortName[cAdds$enabled=="No"],collapse=",")))
        next
      }
      cAddsLess = subset(cAdds,enabled=="Yes")
      if(!is.element(cVal,cAddsLess$shortName)){
        print(paste0(cVal," is already disabled."))
        next
      }

      tt=command(list(delete=NULL,addon=NULL,force=NULL,lib=.filepath(ssimLibrary),name=paste0(cVal,":add-on-transformer")),.session(ssimLibrary))
      retList[[cVal]]=tt
    }

    #ssimLibrary@datasheetNames = .datasheets(ssimLibrary,scope="all",refresh=T)
    return (retList)
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


setMethod('saveDatasheet', signature(ssimObject="SsimLibrary"), function(ssimObject,data,name,project,scenario,append,fileData,forceElements) {
  #ssimObject = newScenario;project=NULL;scenario=NULL;name=sheetName;data=inSheet;fileData=inRasters;append=NULL;forceElements=F
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

  if(!is.null(fileData)&&(length(data)>1)){
    stop("If fileData != NULL, data should be a dataframe, vector, or list of length 1.")
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
    
    #note deletions must happen before files are written.
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
        if(!is.null(fileData)){
          doDelete=T #reset spatial info by deleting the sheet.
        }
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
    
    
    #Write items to appropriate locations
    if(!is.null(fileData)){
      itemNames = names(fileData)
      if(is.null(itemNames)||is.na(itemNames)||(length(itemNames)==0)){
        stop("names(fileData) must be defined, and each element must correspond uniquely to an entry in data")
      }
      
      sheetInfo=subset(datasheet(newScenario,summary=T,optional=T),name==cName)
      fileDir = .filepath(ssimObject)
      if(sheetInfo$isOutput){
        fileDir=paste0(fileDir,".output")
      }else{
        fileDir=paste0(fileDir,".input")
      }
      fileDir=paste0(fileDir,"/Scenario-",.scenarioId(ssimObject),"/",cName)
            
      dir.create(fileDir, showWarnings = FALSE,recursive=T)
      
      for(j in seq(length.out=length(itemNames))){
        #j=1
        cFName = itemNames[j]
        cItem = fileData[[cFName]]
        if(!class(cItem)=="RasterLayer"){
          stop("rsyncrosim currently only supports Raster layers as elements of fileData.")
        }
        #check for cName in datasheet

        findName = cDat==cFName
        findName[is.na(findName)]=F
        sumFind = sum(findName==TRUE,na.rm=T)
        
        if(sumFind>1){
          stop("Each element of names(fileData) must correspond to at most one entry in data. ",sumFind," entries of ",cName," were found in data.")
        }
        if(sumFind==0){
          warning(cName," not found in data. This element will be ignored.")
          next
        }
        
        if(identical(basename(cFName), cFName)){
          cOutName = paste0(fileDir,"/",cFName)
          #cDat[findName]=paste0(fileDir,"/",cDat[findName])
        }else{
          cOutName=cFName
        }
        if(!grepl(".tif",cOutName,fixed=T)){
          cDat[findName]=paste0(cDat[findName],".tif")
          
          cOutName=paste0(cOutName,".tif")
        }
        
        raster::writeRaster(cItem,cOutName,format="GTiff",overwrite=T)
      }
    }
    #cDatHold=cDat
    #cDat=cDatHold
    for(j in seq(length.out=ncol(cDat))){
      if(is.factor(cDat[[j]])){cDat[[j]]=as.character(cDat[[j]])}
      if(is.logical(cDat[[j]])){
        inCol = cDat[[j]]
        cDat[[j]][inCol]="Yes";cDat[[j]][!inCol]="No"
      }
    }
    cDat[,]=as.data.frame(lapply(cDat[,],FUN=function(x) {sapply(x, FUN=function(x){gsub("/","\\",x,fixed=T)})}),stringsAsFactors=F)
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


    print(paste0("Running scenario [",cScn,"] ",name))

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
