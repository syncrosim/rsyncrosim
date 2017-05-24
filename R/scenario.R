# Author: Josie Hughes
# Date : November 2016
# Version 0.1
# Licence GPL v3
#' @include generics.R
#' @include AAAClassDefinitions.R
#' @include ssimLibrary.R
#' @include project.R
NULL
# @name Scenario
# @rdname Scenario-class
setMethod(f='initialize',signature="Scenario",
    definition=function(.Object,ssimLibrary=NULL,project=NULL,name=NULL,id=NULL,sourceScenario=NULL,scenarios=NULL){
    #ssimLibrary = ssimObject; project=cRow$pid;name=cRow$id;id=NULL;scenarios=cRow
    #assume this is being called from scenario fn or getFromXProjScn(). ssimObject and pid are valid, id is valid if not null, and duplicate name problems have been sorted out. 
      
    .Object@breakpoints=list()

    .Object@parentId = 0
    x=ssimLibrary

    #For fast processing - quickly return without system calls if scenario exists
    if(is.null(scenarios)){
      scenarios = getScnSet(x)      
    }
    allScenarios=scenarios
    if(!is.null(name)){cName=name;scenarios=subset(scenarios,name==cName)}
    if(!is.null(id)){cId = id; scenarios=subset(scenarios,id==id)}
    if(!is.null(project)){scenarios=subset(scenarios,pid==project)}
    
    findScn = subset(scenarios,!is.na(id))
    if(nrow(findScn)>1){
      stop("Something is wrong.")
    }

    #If found only one, open it.
    if(nrow(findScn)==1){
      if(!is.null(sourceScenario)){
        stop("Scenario ",name," already exists. Delete the scenario before replacing it.")
      }
      if(findScn$isResult=="Yes"){
        parentBit = strsplit(findScn$name,"[",fixed=T)[[1]][2]
        parent = strsplit(parentBit,"]",fixed=T)[[1]][1]
        .Object@parentId = as.numeric(parent)
      }

      #Go ahead and create the Scenario object without issuing system commands to make sure it is ok
      .Object@session=.session(x)
      .Object@filepath=.filepath(x)
      .Object@datasheetNames = .datasheets(x,scope="all",refresh=T)
      .Object@scenarioId = as.numeric(findScn$id)
      .Object@projectId = as.numeric(findScn$pid)
      return(.Object)
    }
    
    #If given an id for a scenario that does not yet exist, complain
    if(is.null(name)){
      stop(paste0("The library does not contain scenario id ",id,". Please provide a name for the new scenario - the id will be assigned automatically by SyncroSim."))
    }
    
    #Now go ahead to handle odder cases
    #Assume pid is valid, and name is defined, not duplicated, etc
    #x can be either a project or a library - but need a project in order to create a new scenario
    pid=project

    #Create a new scenario
    if(is.null(sourceScenario)){
      tt = command(list(create=NULL,scenario=NULL,lib=.filepath(x),name=name,pid=pid),.session(x))
    }else{
      sid=sourceScenario
      slib=.filepath(x)
      if(class(sourceScenario)=="numeric"){
        if(!is.element(sourceScenario,allScenarios$id)){
          stop("Source scenario id ",sourceScenario," not found in SsimLibrary.")
        }
      }
      if(class(sourceScenario)=="character"){
        sourceOptions = subset(allScenarios,name==sourceScenario)
        if(nrow(sourceOptions)==0){
          stop(paste0("Source scenario name ",sourceScenario," not found in SsimLibrary."))
        }
        if(nrow(sourceOptions)>1){
          stop(paste0("There is more than one scenario called ",sourceScenario," in the SsimLibrary. Please provide a sourceScenario id: ",paste(sourceOptions$id,collapse=",")))
        }
        sid=sourceOptions$id
      }
      if(class(sourceScenario)=="Scenario"){
        sid=.scenarioId(sourceScenario)
        slib=.filepath(sourceScenario)
      }
      tt = command(list(copy=NULL,scenario=NULL,slib=slib,tlib=.filepath(x),name=name,sid=sid,pid=pid),.session(x))
    }
    id = as.numeric(strsplit(tt,": ")[[1]][2])

    .Object@session=.session(x)
    .Object@filepath=.filepath(x)
    .Object@datasheetNames = .datasheets(x,refresh=T,scope="all")
    .Object@scenarioId = as.numeric(id)
    .Object@projectId = as.numeric(pid)
    return(.Object)
  }
)

#' Create or open a scenario or scenarios.
#'
#' If summary = FALSE, returns one or more \code{\link{Scenario}} objects representing a SyncroSim scenarios.
#' If summary = TRUE, returns scenario summary info.
#'
#' @details
#'
#' For each element of scenario:
#' \itemize{
#'   \item {If element/project/ssimObject uniquely identifies an existing scenario: }{Returns the existing Scenario}
#'   \item {If element/project/ssimObject uniquely identifies more than one existing scenario: }{Error}
#'   \item {If element/project/ssimObject do not identify an existing scenario or project: }{Error}
#'   \item {If element/project/ssimObject do not identify an existing scenario and element is numeric: }{Error - a name is required for new scenarios. SyncroSim will automatically assign an id when a scenario is created.}
#'   \item {If element/project/ssimObject do not identify an existing scenario and do identify a project, and element is a character string: }{Creates a new Scenario named element in the project. SyncroSim automatically assigns an id. If sourceScenario is not NULL the new scenario will be a copy of sourceScenario.}
#' }
#'
#' @param ssimObject SsimLibrary/Project or character. An ssimObject containing a filepath to a library, or a filepath.
#' @param scenario Character, integer, or vector of these. Names or ids of one or more scenarios.
#' @param sourceScenario Character or integer. If not NULL, new scenarios will be copies of the sourceScenario.
#' @param summary Logical. If TRUE then loads and returns the scenario(s) in a named vector/dataframe with the scenarioId, name, description, owner, dateModified, readOnly, parentId. Default is TRUE if scenario=NULL, FALSE otherwise.
#' @param results Logical. If TRUE only return result scenarios.
#' @param overwrite Logical. If TRUE, overwrite any existing scenarios. Note that existing scenarios and any associated results will be permanently deleted from the database.
#' @param forceElements Logical. If TRUE then returns a single scenario as a named list; otherwise returns a single scenario as a Scenario object. Applies only when summary=FALSE.
#' @return A \code{Scenario} object representing a SyncroSim scenario, a list of Scenario objects, or a dataframe of scenario names and descriptions.
#' @examples
#' # Create a new scenario
#' myLibrary = ssimLibrary(name="stsim")
#' myProject = project(myLibrary,project="a project") 
#' myScenario = scenario(myProject,scenario="a scenario")
#'
#' @name scenario
# @rdname Scenario-class
#' @export
scenario <- function(ssimObject,scenario=NULL,sourceScenario=NULL,summary=NULL,results=F,overwrite=F,forceElements=F){
  #ssimObject= myProject;scenario="one";sourceScenario="one";summary=NULL;results=F;overwrite=F;forceElements=F
  
  #if ssimObject is a scenario return the scenario
  if(is.element(class(ssimObject),c("Scenario"))&is.null(scenario)){
    if(is.null(summary)){summary=F}
    if(!summary){
      convertObject=T
      returnIds=F
    }else{
      convertObject=F
      returnIds=T
    }
  }else{
    #set summary default
    if(is.null(summary)){
      if(is.null(scenario)){summary=T}else{summary=F}
    }
    convertObject=F
    returnIds=T
  }
  
  xProjScn  =.getFromXProjScn(ssimObject,project=NULL,scenario=scenario,convertObject=convertObject,returnIds=returnIds,goal="scenario",complainIfMissing=F)
  
  if(class(xProjScn)=="Scenario"){
    return(xProjScn)
  }
  
  if(class(xProjScn)!="list"){
    stop("something is wrong")
  }
  ssimObject=xProjScn$ssimObject
  project=xProjScn$project
  scenario=xProjScn$scenario
  allScenarios = xProjScn$scenarioSet
  if(is.element("order",names(allScenarios))){
    scnSet=subset(allScenarios,!is.na(order))
  }else{
    if(nrow(allScenarios)>0){
      allScenarios$order=seq(1,nrow(allScenarios))
    }
    scnSet=allScenarios
  }
  #libScns = scnSet  
  
  if(results){
    scnSet = subset(scnSet,!is.element(isResult,c(NA,F)))
  }
  
  if(nrow(scnSet)==0){
    if(summary){
      scnSet$exists = NULL
      scnSet$order=NULL
      return(scnSet)
    }else{
      stop("Error in scenario(): No scenarios to get or make.") 
    }
  }
  #if all projects exist and summary, simply return summary
  if((sum(is.na(scnSet$exists))==0)&summary){
    scnSet=subset(scnSet,!is.na(order))
    scnSet=scnSet[order(scnSet$order),]
    scnSet$exists = NULL
    scnSet$order=NULL
    return(scnSet)
  }
  
  #Now assume scenario is defined
  #distinguish existing scenarios from those that need to be made
  areIds = is.numeric(scenario)

  #if scenarios need to be made, pass all sceanrios in library
  makeSum = sum(!is.na(scnSet$order)&is.na(scnSet$exists))
  libScns=subset(allScenarios,!is.na(exists))
  if(makeSum>0){
    if(!is.null(sourceScenario)&&(class(sourceScenario)!="Scenario")){
      libScns = getScnSet(ssimObject) #get all scenarios for library, not just those from ssimObject
      #check validity in new("Scenario",...)
    }
  }else{
    if(!is.null(sourceScenario)){
      warning("sourceScenario was ignored because scenario already exists.")
    }
  }

  #make scnenarios/scenario objects
  scnsToMake = subset(scnSet,!is.na(order))
  if(overwrite){
    for (i in seq(length.out=nrow(scnsToMake))){
      if(is.na(scnsToMake$exists[i])){
        next
      }
      ret = removeScenario(ssimObject,scenario=scnsToMake$id[i],force=T)
      cRow=scnsToMake[i,]
      scnsToMake[i,]=NA
      scnsToMake$name[i]=cRow$name;scnsToMake$pid[i]=cRow$pid;scnsToMake$order[i]=cRow$order
    }
  }
  if(summary|results){scnsToMake=subset(scnsToMake,is.na(exists))}
  if(nrow(scnsToMake)==0){
    fullScnSet=fullScnSet[order(fullScnSet$order),]
    fullScnSet$exists=NULL;fullScnSet$order=NULL
    return(fullScnSet)
  }
  if(results&(nrow(scnsToMake)>0)){
    stop(paste0("Could not find these scenarios in the ssimObject. To create them, set results=F: ",paste(scnsToMake$name,collapse=",")))
  }
  scnsToMake=scnsToMake[order(scnsToMake$order),]
  scnList = list()
  for(i in seq(length.out=nrow(scnsToMake))){
    #i = 1
    cRow = scnsToMake[i,]
    if(!is.na(cRow$exists)){
      scnList[[as.character(scnsToMake$id[i])]]=new("Scenario",ssimObject,project=cRow$pid,id=cRow$id,scenarios=cRow)
      
    }else{
      obj=new("Scenario",ssimObject,project=cRow$pid,name=cRow$name,sourceScenario=sourceScenario,scenarios=libScns)
      scnList[[as.character(.scenarioId(obj))]]=obj
    }
  }
  
  if(!summary){
    if((length(scnList)==1)&!forceElements){
      scnList=scnList[[1]]
    }
    return(scnList)
    
  }
  
  scnSetOut=getScnSet(ssimObject)
  scnSetOut$exists=NULL
  idList=data.frame(id = as.numeric(names(scnList)),order=seq(1:length(scnList)))
  scnSetOut =merge(idList,scnSetOut,all.x=T)
  if(sum(is.na(scnSetOut$name))>0){
    stop("Something is wrong with scenario()")
  }
  
  scnSetOut=scnSetOut[order(scnSetOut$order),]
  scnSetOut$order=NULL
  return(scnSetOut)
} 
setMethod('name', signature(ssimObject="Scenario"), function(ssimObject) {
  scnInfo = scenario(ssimObject,summary=T)
  return(scnInfo$name)
})
setReplaceMethod(
  f='name',
  signature="Scenario",
  definition=function(ssimObject,value){
    #x=myScenario;value="New Name"
    tt = command(list(setprop=NULL,lib=.filepath(ssimObject),sid=.scenarioId(ssimObject),name=value),.session(ssimObject))
    if(!identical(tt,"saved")){
      stop(tt)
    }
    return (ssimObject)
  }
)

setMethod('scenarioId', signature(scenario="Scenario"), function(scenario) {
  return(scenario@scenarioId)
})

#' The write status of a Scenario
#'
#' Whether or not the scenario is readOnly
#'
#' @param x An Scenario object.
#' @return TRUE or FALSE
#' @export
setGeneric('readOnly',function(x) standardGeneric('readOnly'))
setMethod('readOnly', signature(x="Scenario"), function(x) {
  #x=myScenario
  info = scenario(x,summary=T)
  answer = info$readOnly[info$id==.scenarioId(x)]=="Yes"
  return(answer)
})

#' The author of a Scenario
#'
#' @param x An Scenario object.
#' @return The author name.
#' @export
setGeneric('author',function(x) standardGeneric('author'))
setMethod('author', signature(x="Scenario"), function(x) {
  #x=myScenario
  info = scenario(x,summary=T)
  answer = info$author[info$id==.scenarioId(x)]
  return(answer)
})

#' The description of a Scenario
#'
#' @param x An Scenario object.
#' @return The description.
#' @export
setGeneric('description',function(x) standardGeneric('description'))
setMethod('description', signature(x="Scenario"), function(x) {
  #x=myScenario
  info = scenario(x,summary=T)
  answer = info$description[info$id==.scenarioId(x)]
  return(answer)
})

#' Set the properties of a scenario.
#'
#' Set the author, description and/or readOnly status of a scenario.
#'
#' @param x An Scenario object.
#' @param author An author name.
#' @param description A description.
#' @param readOnly TRUE or FALSE.
#' @return "saved" or a failure message
#' @export
setGeneric('setProperties',function(x,author=NULL,description=NULL,readOnly=NULL) standardGeneric('setProperties'))
setMethod('setProperties', signature(x="Scenario"), function(x,author,description,readOnly) {
  #x=myScenario
  propertyArgs = list(setprop=NULL,lib=.filepath(x),sid=.scenarioId(x))
  if(!is.null(author)){propertyArgs$author=author}
  if(!is.null(description)){propertyArgs$description=description}
  if(!is.null(readOnly)){
    if(readOnly){propertyArgs$readonly="yes"}else{
      propertyArgs$readonly="no"
    }
  }
  if(length(propertyArgs)==3){
    stop("Specify at least one of author, description and readOnly")
  }

  tt = command(propertyArgs,.session(x))
  return(tt)
})


#' The parent scenario id of a SyncroSim Scenario.
#'
#' The id of the parent of a SyncroSim results scenario.
#' NA if scenario is not a results scenario.
#'
#' @param scenario A Scenario object.
#' @export
setGeneric('parentId',function(scenario) standardGeneric('parentId'))
setMethod('parentId', signature(scenario="Scenario"), function(scenario) {
  if(scenario@parentId==0){return(NA)}
  return(scenario@parentId)
})

setMethod('projectId', signature(ssimObject="Scenario"), function(ssimObject) {
  return(ssimObject@projectId)
})

setMethod('multiband', signature(x="Scenario"), function(x,action,grouping) {
  #x=myResult;action="rebuild";grouping=NULL
  if(is.na(parentId(x))){
    stop("Need a result Scenario.")
  }

  #command(c("help"),program="SyncroSim.MultiBand.exe")
  args = list(lib=.filepath(x),sid=.scenarioId(x))
  args[[action]]=NA
  if(action=="apply"){
    if(!is.null(grouping)){
      args$grp = grouping
    }
  }
  tt = command(args,.session(x),program="SyncroSim.MultiBand.exe")
  return(tt)
})

setMethod('spatialData', signature(x="Scenario"), function(x,sheet,iterations,timesteps,nameFilters,rat) {
  # x= myResult[[1]]; sheet="STSim_InitialConditionsSpatial";iterations=NULL;timesteps = NULL;rat=NULL;nameFilters=NULL

  cSheets = .datasheets(x)
  if(!is.element(sheet,cSheets$name)){
    cSheets = .datasheets(x,refresh=T)
  }
  #cSheets=subset(cSheets,isSpatial)
  #if(!is.element(sheet,cSheets$name)){
  #  stop(sheet," is not a spatial data sheet.")
  #}

  #TO DO: make sure datasheet is spatial after opening
  cMeta = datasheet(x,name=sheet,optional=T)

  if(nrow(cMeta)==0){
    multiband(x,action="rebuild")
    cMeta = datasheet(x,name=sheet,optional=T)
  }

  tryCount = 0
  while(tryCount <=1){
    expectCols = c("Iteration","Timestep","Filename","Band","outName")
    checkCols = setdiff(names(cMeta),expectCols)
    for(i in seq(length.out=length(checkCols))){
      #i=1
      cSum = sum(!is.na(cMeta[[checkCols[i]]]))
      if(cSum==0){
        cMeta[[checkCols[i]]]=NULL
      }
    }

    warningMsg = ""
    if(!is.null(timesteps)&is.element("Timestep",names(cMeta))){
      timesteps=as.numeric(timesteps)
      missSteps = setdiff(timesteps,cMeta$Timestep)
      if(length(missSteps)>0){
        warningMsg = paste0("Selected timesteps not available: ",paste(missSteps,collapse=","))
      }
      cMeta = subset(cMeta,is.element(Timestep,timesteps))
    }

    if(!is.null(iterations)&is.element("Iteration",names(cMeta))){
      iterations=as.numeric(iterations)
      missSteps = setdiff(iterations,cMeta$Iteration)
      if(length(missSteps)>0){
        warningMsg = paste0(warningMsg," Selected iterations not available: ",paste(missSteps,collapse=","))
      }
      cMeta = subset(cMeta,is.element(Iteration,iterations))
    }

    if((nchar(warningMsg)>0)|(nrow(cMeta)==0)){
      if(tryCount == 1){
        if(nrow(cMeta)==0){stop("No data available.")}else{
          warning(warningMsg)
        }
      }else{
        multiband(x,action="rebuild")
        cMeta = datasheet(x,name=sheet,optional=T)
      }
    }
    tryCount = tryCount+1
  }
  if(!is.element("Filename",names(cMeta))){
    tempFilename = names(cMeta)[grepl("FileName",names(cMeta))]
    if(length(tempFilename)==1){
      names(cMeta)[names(cMeta)==tempFilename]="Filename"
      cMeta$Band=NA

      cMeta$Filename = paste0(.filepath(x),".input/Scenario-",.scenarioId(x),"/",sheet,"/",cMeta$Filename)

    }
  }

  if(!is.element("Filename",names(cMeta))){
    if(nrow(cMeta)>1){
      stop("Handle this case.")
    }
    #handle spatial inputs by making cMeta table of correct format

    cMIn = cMeta
    cFNames = data.frame(t(cMeta[1,]),stringsAsFactors=F)
    names(cFNames)=c("Filename")
    cFNames = subset(cFNames,!is.na(Filename))
    cFNames$Band=NA
    cMeta =cFNames
    cMeta$outName = paste0(sheet,".Scn",.scenarioId(x),".",gsub(".tif","",cMeta$Filename,fixed=T))
    cMeta$Filename = paste0(.filepath(x),".input/Scenario-",.scenarioId(x),"/",sheet,"/",cMeta$Filename)

  }else{
    cMeta$outName = paste0(sheet,".Scn",.scenarioId(x),".It",cMeta$Iteration,".Ts",cMeta$Timestep)

  }
  otherCols = setdiff(names(cMeta),expectCols)
  for(i in seq(length.out=length(otherCols))){
    #special characters not tolerated in titles
    addBit = gsub(" ","",cMeta[[otherCols[i]]],fixed=T)
    addBit = gsub("-","",addBit,fixed=T)
    cMeta$outName=paste0(cMeta$outName,".",addBit)
  }

  if(!is.null(nameFilters)){
    for(i in seq(length.out=length(nameFilters))){
      cMeta= subset(cMeta,grepl(nameFilters[i],cMeta$outName,fixed=T))
    }
  }

  nFiles = unique(cMeta$Filename)
  if((length(nFiles)==1)&(nrow(cMeta)>1)){
    if(!file.exists(nFiles)){
      #TO DO: path should already be there...
      addPath = paste0(.filepath(x),".output/Scenario-",.scenarioId(x),"/Spatial/",nFiles)
      if(!file.exists(addPath)){
        stop("Output not found: ",nFiles)
      }
      cMeta$Filename=addPath
    }

    cStack=raster::brick(cMeta$Filename[1])

    cMeta$layerName = paste0(strsplit(nFiles,".",fixed=T)[[1]][1],".",cMeta$Band)

    keepLayers = intersect(names(cStack),cMeta$layerName)
    cStack = raster::subset(cStack,keepLayers)
    missing=setdiff(cMeta$layerName,names(cStack))
    if(length(missing)>0){
      warning("Some layers not found: ",paste(cMeta$outName[is.element(cMeta$layerName,missing)]))
    }
    cMeta=subset(cMeta,is.element(layerName,names(cStack)))

    for(i in 1:nrow(cMeta)){
      #i =1

      cRow =cMeta[i,]
      cName = cRow$layerName


      if(!is.null(rat)){
        obsVals = freq(cStack[[cName]])[,"value"]
        missingVals = setdiff(obsVals,c(NA,rat$ID))
        if(length(missingVals)>0){
          stop("Raster values not found in legend$ID: ",paste(missingVals,collapse=","))
        }
        #NOTE raster objects have a legend class but methods not yet implemented, except can store a color table
        #See colortable() for details
        rasterAttributes(cStack[[cName]])=rat

      }
      cStack[[cName]]@title = cRow$outName

      names(cStack)[names(cStack)==cRow$layerName]=cRow$outName

    }
    #cStack = raster::brick(cStack)
  }else{

    for(i in 1:nrow(cMeta)){
      #i =1

      #install.packages("rgdal")
      cRow =cMeta[i,]
      if(is.na(cRow$Filename)){next}
      if(!file.exists(cRow$Filename)){
        #TO DO: path should already be there...
        addPath = paste0(.filepath(x),".output/Scenario-",.scenarioId(x),"/Spatial/",cRow$Filename)
        if(!file.exists(addPath)){
          stop("Output not found: ",cRow$Filename)
        }
        cRow$Filename=addPath
      }
      if(is.na(cRow$Band)){
        cRaster = raster::raster(cRow$Filename)
      }else{
        cRaster= raster::raster(cRow$Filename,band=cRow$Band)
      }

      if(!is.null(rat)){
        obsVals = freq(cRaster)[,"value"]
        missingVals = setdiff(obsVals,c(NA,rat$ID))
        if(length(missingVals)>0){
          stop("Raster values not found in legend$ID: ",paste(missingVals,collapse=","))
        }
        #NOTE raster objects have a legend class but methods not yet implemented, except can store a color table
        #See colortable() for details
        rasterAttributes(cRaster)=rat
      }
      cRaster@title = cRow$outName
      if(i==1){
        cStack = raster::stack(cRaster)
        names(cStack) = c(cRow$outName)
        #TO DO: consider options for storing this info in a less hokey way
      }else{
        oldNames = names(cStack)
        cStack = raster::addLayer(cStack,cRaster)
        names(cStack)=c(oldNames,cRow$outName)
      }
    }
  }
  return(cStack)
})

setMethod('loadSpatialData', signature(x="SsimLibrary"), function(x,data,metadata,project,scenario,breakpoint,check) {
  #x = myScenario;project=NULL;scenario=NULL;metadata=metadata;data=data;breakpoint=F;check=T
  #.filepath=filepath;.id=id
  x = .getFromXProjScn(x,project,scenario,returnIds=F,convertObject=T,complainIfMissing=T,goal="Scenario")
  #Expecting a single scenario
  if(class(x)!="Scenario"){
    stop("Expecting a x/project/scenario to identify a single scenario in loadSpatialData()")
  }
  
  # get metadata
  if(is.null(metadata)){
    stop("Get metadata from names(data)")
  }
  
  if(nrow(metadata)==0){
    stop("Expecting metadata or names(data): loadDatasheets")
  }
  
  if(length(unique(metadata$SheetName))>1){
    stop("Metadata can contain only one SheetName.")
  }
  cSheetName =  metadata$SheetName[1];metadata$SheetName=NULL
  if(is.null(cSheetName)){
    stop("metadata should include 'SheetName' column")
  }
  fileCols = names(metadata)[grepl("FileName",names(metadata),fixed=T)]
  if(check|(!breakpoint)){
    #Check that metadata is valid
    cSheet = datasheet(x,cSheetName,optional=T)
    empty = subset(cSheet,cSheet[[1]]=="not likely")
    
    check = try(addRows(empty,subset(metadata,select=setdiff(names(metadata),c("RasterLayerName")))))
    
    if(inherits(check, "try-error")){
      stop("Metadata is not valid. Unexpected columns include: ",paste(setdiff(names(metadata),c("RasterLayerName",names(cSheet))),collapse=","))
    }
    #if non-FileName columns differ then append. Otherwise overwrite.
    
    inF = subset(cSheet,select=setdiff(names(cSheet),fileCols))
    outF = subset(check,select=setdiff(names(check),fileCols))
    
    outF$isOut = 1
    cSheet = merge(cSheet,outF,all.x=T)
    cSheet = subset(cSheet,is.na(isOut));cSheet$isOut=NULL
    check = addRows(cSheet,subset(metadata,select=setdiff(names(metadata),c("RasterLayerName"))))
  }
  if(breakpoint){
    outDir = paste0(.filepath(x),'.temp/Data')
  }else{
    outDir=paste0(.filepath(x),".input/Scenario-",.scenarioId(x),"/",cSheetName)
  }
  dir.create(outDir, showWarnings = FALSE,recursive=T)
  
  cMeta = metadata
  #There can be more than one FileName column - make long file
  
  if(length(fileCols)==1){
    #names(cMeta)[names(cMeta)==fileCols]="FileName"
    FileCol = fileCols
  }else{
    #Make wide file long
    #Make a long file by brute force - because reshape() function in the base package is awful
    for(i in 1:length(fileCols)){
      #i=1
      cRow = subset(cMeta,select=c(fileCols[i],setdiff(names(cMeta),fileCols)))
      names(cRow)[names(cRow)==fileCols[i]]="FileName"
      if(i==1){
        cMTemp=cRow
      }else{
        cMTemp=rbind(cMTemp,cRow)
      }
    }
    fileCol="FileName"
    cMeta=cMTemp
  }
  if(!is.element("RasterLayerName",names(cMeta))){
    cMeta$RasterLayerName = gsub("-",".",cMeta[[FileCol]],fixed=T)
  }
  cMeta[[FileCol]]=as.character(cMeta[[FileCol]])
  for(i in 1:nrow(cMeta)){
    #i =1
    cRow = cMeta[i,]
    cDat = data[[cRow$RasterLayerName]]
    cRow$RasterLayerName=NULL
    cFileCol = names(cRow)[grepl("FileName",names(cRow))]
    cRow[[cFileCol]] = basename(cRow[[cFileCol]])
    cRow[[cFileCol]] = paste0(outDir,"/",cRow[[cFileCol]])
    
    if(!breakpoint){
      raster::writeRaster(cDat, filename=cRow[[cFileCol]], format="GTiff", overwrite=TRUE)
      
    }else{
      if(cSheetName != "STSim_TransitionSpatialMultiplier"){
        stop("Handle this: loadSpatialData when sheet name != STSim_TransitionSpatialMultiplier")
      }
      raster::writeRaster(cDat,cRow[[cFileCol]],overwrite=T)
      saveDatasheet(x,cRow,name=cSheetName,breakpoint=T)
    }
  }
  if(!breakpoint){
    saveDatasheet(x,check,name=cSheetName)
  }
  
  spatialProperties=data.frame(NumRows=dim(cDat)[1],
                               NumColumns=dim(cDat)[2],
                               NumCells=dim(cDat)[1]*dim(cDat)[2],
                               CellSize=res(cDat)[1],
                               XLLCorner=xmin(cDat),
                               YLLCorner=ymin(cDat))
                               
  return(spatialProperties)
  
})

# Set breakpoint of a Scenario.
#
# Add a Breakpoint object to breakpoints of a Scenario.
#
# @param x A SyncroSim Scenario
# @param breakpointType bi: before iteration; ai: after iteration; bt:before timestep; at: aftertimestep
# @param transformerName 'stsim:core-transformer' or?
# @param arguments A vector of timesteps or iterations e.g. c(1,2)
# @param callback The function to apply. See STSimBreakpointsTutorial.R for details.
# @return An SyncroSim Scenario object containing breakpoints
# @export
if(0){
setGeneric('setBreakpoint',function(x,breakpointType,transformerName,arguments,callback) standardGeneric('setBreakpoint'))
setMethod('setBreakpoint',signature(x="Scenario"),function(x,breakpointType, transformerName, arguments, callback) {
  #x=myScenario
  types = list(bi = 'syncrosim-stochastic-time:break-before-iteration',
               ai = 'syncrosim-stochastic-time:break-after-iteration',
               bt = 'syncrosim-stochastic-time:break-before-timestep',
               at = 'syncrosim-stochastic-time:break-after-timestep')

  if(!is.element(breakpointType,names(types))){
    stop("breakpointType not recognized: ",breakpointType)
  }
  breakpointName = types[[breakpointType]]
  if(is.element(breakpointName,names(breakpoints(x)))){
    warning('Resetting breakpoint for: ', breakpointName,' -> ',transformerName)
  }
  x@breakpoints[[breakpointName]] = breakpoint(breakpointName,transformerName,arguments,callback)
  return(x)
})
}

# The breakpoints of a Scenario
#
# The breakpoints of a Scenario
# @param x A Scenario object.
# @return A list of Breakpoint objects.
# @export
setGeneric('breakpoints',function(x) standardGeneric('breakpoints'))
setMethod('breakpoints', signature(x="Scenario"), function(x) {
  #x=myScenario
  return(x@breakpoints)
})



