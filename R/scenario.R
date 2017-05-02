# Author: Josie Hughes
# Date : November 2016
# Version 0.1
# Licence GPL v3
#' @include generics.R
#' @include ssimLibrary.R
#' @include project.R
NULL
#' SyncroSim Scenario class
#'
#' \code{Scenario} object representing a SyncroSim Project.
#'
#' @seealso See \code{\link{scenario}} for options when creating or loading an SyncroSim Scenario.
#' @slot session The session associated with the library.
#' @slot filepath The path to the library on disk.
#' @slot datasheetNames Names and scope of all datasheets in library.
#' @slot pid The project id.
#' @slot name The scenario name.
#' @slot id The scenario id.
#' @slot parentId For a result scenario, this is the id of the parent scenario. 0 indicates this is not a result scenario.
#' @slot breakpoints An (optional) list of Breakpoint objects. See ?breakpoints for details.
#' @name Scenario-class
#' @rdname Scenario-class
#' @export Scenario
Scenario <- setClass("Scenario", contains="SsimLibrary",representation(pid="numeric",name="character",id="numeric",parentId="numeric",breakpoints="list"))
# @name Scenario
# @rdname Scenario-class
setMethod(f='initialize',signature="Scenario",
    definition=function(.Object,ssimLibrary=NULL,project=NULL,name=NULL,id=NULL,create=T,scenarios=NULL,sourceScenario=NULL,author=NULL,description=NULL,readOnly=NULL){
    #ssimLibrary = myLibrary  #.project(myLibrary,id=1)#ssimLibrary(model="stsim", name= "C:/Temp/NewLibrary.ssim",session=devSsim)
    # id=NULL;name=NULL;project=NULL;scenarios=NULL;create=T;sourceScenario=NULL
    if(is.character(id)){id = as.numeric(id)}

    .Object@breakpoints=list()

    .Object@parentId = 0
    x=NULL
    if(!is.null(ssimLibrary)){
      x=ssimLibrary
    }else{
      x=project
    }
    if(is.null(x)){
      stop("Specify a library and (optional) project to create or open a scenario.")
    }
    if(is.character(x)){
      x=.ssimLibrary(name=x)
    }

    #For fast processing - quickly return without system calls if scenario exists and can be easily identified
    if(is.null(scenarios)){
      scenarios = .scenarios(x,names=T)
    }
    findScn = scenarios
    if(!is.null(name)){
      cName=name
      findScn = subset(findScn,name==cName)
    }
    if(!is.null(id)){
      cId = as.character(id)
      findScn = subset(findScn,id==cId)
    }
    pid=project
    if(class(x)=="Project"){
      pid = .id(x)
    }
    if(class(project)=="Project"){
      pid = .id(project)
    }
    if(!is.null(pid)){
      cPid = as.character(pid)
      findScn = subset(findScn,pid==cPid)
    }
    cProjects=NULL
    if((nrow(findScn)!=1)&&(class(pid)=="character")){
      cProjects = projects(x,names=T)
      findProject = subset(cProjects,name==project)
      findScn = subset(findScn,is.element(pid,findProject$id))
      if(nrow(findProject)>0){
        pid = as.numeric(findProject$id)
      }
    }
    #If found only one, open it.
    if(nrow(findScn)==1){name=findScn$name}

    if(is.null(id)&is.null(name)&(nrow(findScn)>0)){
        name = "Scenario"
        cName=name
        findScn = subset(findScn,name==cName)
    }

    propertyArgs = list(setprop=NULL,lib=.filepath(x))
    if(!is.null(author)){propertyArgs$author=author}
    if(!is.null(description)){propertyArgs$description=description}
    if(!is.null(readOnly)){
      if(readOnly){propertyArgs$readonly="yes"}else{
        propertyArgs$readonly="no"
      }
    }

    if(nrow(findScn)==1){
      if(!is.null(sourceScenario)){
        stop("Scenario ",name," already exists. Delete the scenario before replacing it.")
      }
      if(findScn$isResult=="Yes"){
        parentBit = strsplit(findScn$name,"[",fixed=T)[[1]][2]
        parent = strsplit(parentBit,"]",fixed=T)[[1]][1]
        .Object@parentId = as.numeric(parent)
      }
      if(length(propertyArgs)>2){
        propertyArgs$sid = findScn$id
        tt = command(propertyArgs,.session(x))
        if(tt!="saved"){
          stop("Failed to set properties:",tt)
        }
      }

      #Go ahead and create the Scenario object without issuing system commands to make sure it is ok
      .Object@session=.session(x)
      .Object@filepath=.filepath(x)
      .Object@datasheetNames = .datasheets(x,scope="all",refresh=T)
      .Object@id = as.numeric(findScn$id)
      .Object@name = findScn$name
      .Object@pid = as.numeric(findScn$pid)
      return(.Object)
    }

    #Now go ahead to handle odder cases
    #x can be either a project or a library - but need a project in order to create a new scenario

    if(create){
      if(!is.null(pid)&(class(x)=="SsimLibrary")){
        if(length(pid)>1){
          stop(paste0("The library contains more than one project called ",project,". Specify a project id:",paste(pid,collapse=",")))
        }
        if(class(pid)=="numeric"){
          x = .project(x,id=pid)
        }else{
          x = .project(x,name=pid)
        }
      }
    }

    #if given a library, can only open an existing scenario
    if((class(x)=="SsimLibrary")||!create||(nrow(findScn)>0)){
      if(nrow(findScn)==0){
        if(!create){
          stop(paste0("Scenario ",name," (id=",id,") does not exist. Provide a project and set create=T to create a new scenario."))
        }
        if(is.null(cProjects)){
          cProjects = projects(x,names=T)
        }
        if(nrow(cProjects)>1){
          stop(paste0("Scenario ",name," (id=",id,") does not exist. Provide a project to create a new scenario."))
        }else{
          x = project(x)
        }
      }
      if(nrow(findScn)>1){
        stop(paste0("More than one scenario was identified. Please provide more information. See scenarios(x,names=T) for options."))
      }
      if(class(x)!="Project"){
        stop("Something is wrong. Can't identify a unique existing scenario, and need a project to create a new scenario")
      }
    }

    #Now assume we have a project, and are permitted to create a new scenario
    pid=.id(x) #If pid conflicts with project id, ignore pid.
    if(nrow(findScn)>0){stop("Something is wrong")}

    #If given an id for a scenario that does not yet exist, complain
    if(!is.null(id)){
      stop(paste0("The library does not contain scenario id ",id,". Please provide a name for the new scenario - the id will be assigned automatically by SyncroSim."))
    }

    #Create a new scenario
    if(is.null(name)){
      #allScenarios = scenarios(.ssimLibrary(x),names=T)
      #if(nrow(allScenarios)==0){
      #  name = "Scenario1"
      #}else{
      #  name =paste0("Scenario",max(allScenarios$id)+1)
      #}
      name="Scenario"
    }
    if(is.null(sourceScenario)){
      tt = command(list(create=NULL,scenario=NULL,lib=.filepath(x),name=name,pid=pid),.session(x))
    }else{
      if(is.character(sourceScenario)){
        findSource = subset(scenarios,name==sourceScenario)
      }else{
        findSource = subset(scenarios,id==sourceScenario)
      }
      findSource=subset(findSource,isResult=No)
      if(nrow(findSource)==0){
        stop("Source scenario ",sourceScenario," not found.")
      }
      if(nrow(findSource)>1){
        stop("There is more than one scenario named ",sourceScenario,". Specify an id: ",paste(findSource$id,collapse=","))
      }
      tt = command(list(copy=NULL,scenario=NULL,lib=.filepath(x),name=name,sid=findSource$id),.session(x))
    }
    id = as.numeric(strsplit(tt,": ")[[1]][2])

    if(length(propertyArgs)>2){
      propertyArgs$sid = id
      tt = command(propertyArgs,.session(x))
    }
    .Object@session=.session(x)
    .Object@filepath=.filepath(x)
    .Object@datasheetNames = .datasheets(x,refresh=T,scope="all")
    .Object@id = as.numeric(id)
    .Object@name = name
    .Object@pid = as.numeric(pid)
    return(.Object)
  }
)
#' Create or open a scenario
#'
#' Creates or opens a \code{\link{Scenario}} object representing a SyncroSim scenario.
#' @details
#'
#' \itemize{
#'   \item {If name/id/project uniquely identifies an existing scenario: }{Returns the existing Scenario}
#'   \item {If name/id/project uniquely identifies more than one existing scenario: }{Error}
#'   \item {If project is NULL, and name/id do not uniquely idenfity an existing scenario: }{Error}
#'   \item {If project is not NULL, name is NULL, and id/project do not idenfity an existing scenario: }{Creates a new Scenario called "Scenario". The id argument is ignored, as SyncroSim automatically assigns an id. If sourceScenario is not NULL the new scenario will be a copy of sourceScenario.}
#'   \item {If project is not NULL, name is not NULL, and name/id/project do not idenfity an existing scenario: }{Creates a new Scenario called <name>. The id argument is ignored, as SyncroSim automatically assigns an id. If sourceScenario is not NULL the new scenario will be a copy of sourceScenario.}
#' }
#'
#' @param ssimLibrary An SsimLibrary object or name, or an object that contains an SsimLibrary. If a name is given, the library will be opened using the default session.
#' @param project A Project object, project name, or project id.
#' @param name The scenario name.
#' @param id The scenario id.
#' @param create If TRUE, create scenario if one does not exist. If FALSE, only return an existing scenario
#' @param scenarios A dataframe of existing scenarios produced by scenarios(). Use to speed processing.
#' @param sourceScenario The name or id of a scenario to copy.
#' @param author Optional.
#' @param description Optional.
#' @param readOnly By default scenarios are not readOnly.
#' @return A \code{Scenario} object representing a SyncroSim scenario.
#' @examples
#' # Create a new default scenario
#' myLibrary = ssimLibrary(model="stsim",name="stsim")
#' myProject = project(myLibrary) #If no name is given, creates a project named "Project".
#' myScenario = scenario(myProject)
#'
#' @name scenario
# @rdname Scenario-class
#' @export
scenario <- function(ssimLibrary=NULL,project=NULL,name=NULL,id=NULL,create=T,scenarios=NULL,sourceScenario=NULL,author=NULL,description=NULL,readOnly=NULL) new("Scenario",ssimLibrary,project,name,id,create,scenarios,sourceScenario,author,description,readOnly)

setMethod('name', signature(x="Scenario"), function(x) {
  return(x@name)
})
setReplaceMethod(
  f='name',
  signature="Scenario",
  definition=function(x,value){
    #x=myScenario;value="New Name"
    tt = command(list(setprop=NULL,lib=.filepath(x),sid=.id(x),name=value),.session(x))
    if(!identical(tt,"saved")){
      stop(tt)
    }
    x@name = value
    return (x)
  }
)

setMethod('id', signature(x="Scenario"), function(x) {
  return(x@id)
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
  info = scenarios(x,names=T)
  answer = info$readOnly[info$id==.id(x)]=="Yes"
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
  info = scenarios(x,names=T)
  answer = info$author[info$id==.id(x)]
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
  info = scenarios(x,names=T)
  answer = info$description[info$id==.id(x)]
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
  propertyArgs = list(setprop=NULL,lib=.filepath(x),sid=.id(x))
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
#' 0 if x is not a results scenario.
#'
#' @param x A Scenario object.
#' @export
setGeneric('parentId',function(x) standardGeneric('parentId'))
setMethod('parentId', signature(x="Scenario"), function(x) {
  return(x@parentId)
})


#' The pid of a SyncroSim Scenario.
#'
#' The project id of a SyncroSim Scenario
#'
#' @param x An Scenario object.
#' @export
setGeneric('pid',function(x) standardGeneric('pid'))
setMethod('pid', signature(x="Scenario"), function(x) {
  return(x@pid)
})
#' The project id of a SyncroSim Scenario.
#'
#' The project id of a SyncroSim Scenario
#'
#' @param x An Scenario object.
#' @export
projectId = pid

#' @describeIn ssimLibrary Get the SsimLibrary associated with a SyncroSim Scenario.
setMethod('ssimLibrary', signature(name="Scenario"), function(name) {
  #model=cScn
  out = .ssimLibrary(name=.filepath(name),session=.session(name))
  return(out)
})

setMethod('multiband', signature(x="Scenario"), function(x,action,grouping) {
  #x=myResult;action="rebuild";grouping=NULL
  if(parentId(x)==0){
    stop("Need a result Scenario.")
  }

  #command(c("help"),program="/SyncroSim.MultiBand.exe")
  args = list(lib=.filepath(x),sid=.id(x))
  args[[action]]=NA
  if(action=="apply"){
    if(!is.null(grouping)){
      args$grp = grouping
    }
  }
  tt = command(args,.session(x),program="/SyncroSim.MultiBand.exe")
  return(tt)
})

setMethod('spatialData', signature(x="Scenario"), function(x,sheet,iterations,timesteps,nameFilters,rat) {
  # x= myResult[[1]]; sheet="STSim_InitialConditionsSpatial";iterations=NULL;timesteps = NULL;rat=NULL;nameFilters=NULL

  cSheets = datasheets(x)
  if(!is.element(sheet,cSheets$name)){
    cSheets = datasheets(x,refresh=T)
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

      cMeta$Filename = paste0(.filepath(x),".input/Scenario-",.id(x),"/",sheet,"/",cMeta$Filename)

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
    cMeta$outName = paste0(sheet,".Scn",.id(x),".",gsub(".tif","",cMeta$Filename,fixed=T))
    cMeta$Filename = paste0(.filepath(x),".input/Scenario-",.id(x),"/",sheet,"/",cMeta$Filename)

  }else{
    cMeta$outName = paste0(sheet,".Scn",.id(x),".It",cMeta$Iteration,".Ts",cMeta$Timestep)

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
      addPath = paste0(.filepath(x),".output/Scenario-",.id(x),"/Spatial/",nFiles)
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
        addPath = paste0(.filepath(x),".output/Scenario-",.id(x),"/Spatial/",cRow$Filename)
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
  x = .getFromXProjScn(x,project,scenario)
  
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
    
    check = try('addRows<-'(empty,subset(metadata,select=setdiff(names(metadata),c("RasterLayerName")))))
    
    if(inherits(check, "try-error")){
      stop("Metadata is not valid. Unexpected columns include: ",paste(setdiff(names(metadata),c("RasterLayerName",names(cSheet))),collapse=","))
    }
    #if non-FileName columns differ then append. Otherwise overwrite.
    
    inF = subset(cSheet,select=setdiff(names(cSheet),fileCols))
    outF = subset(check,select=setdiff(names(check),fileCols))
    
    outF$isOut = 1
    cSheet = merge(cSheet,outF,all.x=T)
    cSheet = subset(cSheet,is.na(isOut));cSheet$isOut=NULL
    check = 'addRows<-'(cSheet,subset(metadata,select=setdiff(names(metadata),c("RasterLayerName"))))
  }
  if(breakpoint){
    outDir = paste0(.filepath(x),'.temp/Data')
  }else{
    outDir=paste0(.filepath(x),".input/Scenario-",.id(x),"/",cSheetName)
  }
  dir.create(outDir, showWarnings = FALSE,recursive=T)
  
  cMeta = metadata
  #There can be more than one FileName column - make long file
  
  if(length(fileCols)==1){
    names(cMeta)[names(cMeta)==fileCols]="FileName"
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
    cMeta=cMTemp
  }
  if(!is.element("RasterLayerName",names(cMeta))){
    cMeta$RasterLayerName = gsub("-",".",cMeta$FileName,fixed=T)
  }
  cMeta$FileName=as.character(cMeta$FileName)
  for(i in 1:nrow(cMeta)){
    #i =1
    cRow = cMeta[i,]
    cDat = data[[cRow$RasterLayerName]]
    cRow$RasterLayerName=NULL
    cFileCol = "FileName"#names(cRow)[grepl("FileName",names(cRow))]
    cRow[[cFileCol]] = basename(cRow[[cFileCol]])
    cRow[[cFileCol]] = paste0(outDir,"/",cRow[[cFileCol]])
    
    if(!breakpoint){
      raster::writeRaster(cDat, filename=cRow[[cFileCol]], format="GTiff", overwrite=TRUE)
      
    }else{
      if(cSheetName != "STSim_TransitionSpatialMultiplier"){
        stop("Handle this: loadSpatialData when sheet name != STSim_TransitionSpatialMultiplier")
      }
      raster::writeRaster(cDat,cRow[[cFileCol]],overwrite=T)
      loadDatasheets(x,cRow,name=cSheetName,breakpoint=T)
    }
  }
  if(!breakpoint){
    loadDatasheets(x,check,name=cSheetName)
  }
  
  spatialProperties=data.frame(NumRows=dim(cDat)[1],
                               NumColumns=dim(cDat)[2],
                               NumCells=dim(cDat)[1]*dim(cDat)[2],
                               CellSize=res(cDat)[1],
                               XLLCorner=xmin(cDat),
                               YLLCorner=ymin(cDat))
                               
  return(spatialProperties)
  
})


#' Set breakpoint of a Scenario.
#'
#' Add a Breakpoint object to breakpoints of a Scenario.
#'
#' @param x A SyncroSim Scenario
#' @param breakpointType bi: before iteration; ai: after iteration; bt:before timestep; at: aftertimestep
#' @param transformerName 'stsim:core-transformer' or?
#' @param arguments A vector of timesteps or iterations e.g. c(1,2)
#' @param callback The function to apply. See STSimBreakpointsTutorial.R for details.
#' @return An SyncroSim Scenario object containing breakpoints
#' @export
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

#' The breakpoints of a Scenario
#'
#' The breakpoints of a Scenario
#' @param x A Scenario object.
#' @return A list of Breakpoint objects.
#' @export
setGeneric('breakpoints',function(x) standardGeneric('breakpoints'))
setMethod('breakpoints', signature(x="Scenario"), function(x) {
  #x=myScenario
  return(x@breakpoints)
})



