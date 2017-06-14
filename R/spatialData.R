# Copyright © 2017 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Get spatial inputs or outputs from a SyncroSim scenario.
#'
#' Get spatial inputs or outputs from a SyncroSim scenario.
#' @details
#'
#' The Color column of a rat table should have one of these formats:
#' \itemize{
#'   \item {R,G,B,alpha: } {4 numbers representing red, green, blue and alpha, separated by commas, and scaled between 0 and 255. See rgb() for details.}
#'   \item {R colour names: } {See colors() for options.}
#'   \item {hexadecimal colors: } {As returned by R functions such as rainbow(), heat.colors(), terrain.colors(), topo.colors(), gray(), etc.}
#' }
#'
#' The names() of the returned raster stack contain metadata.
#' For datasheets without Filename this is: paste0(<datasheet name>,".Scn",<scenario id>,".",<tif name>)
#' For datasheets containing Filename this is: paste0(<datasheet name>,".Scn",<scenario id>,".It",<iteration>,".Ts",<timestep>)
#'
#' @param x A SyncroSim results Scenario or list of SyncroSim result Scenarios.
#' @param sheet The name of a spatial datasheet.
#' @param iterations A vector of iterations. If NULL(default) all available iterations will be included
#' @param nameFilters A vector of strings. Only layer name that include these terms will be returned.
#' @param timesteps A vector of timesteps. If NULL(default) all available timesteps will be included.
#' @param rat An (optional) raster attribute table. This is dataframe with ID, (optional) Color, and other columns. See raster::ratify() for details.
#' @return A RasterStack or RasterBrick object. See raster package documentation for details.
#' @export
setGeneric('spatialData',function(x,sheet,iterations=NULL,timesteps=NULL,nameFilters=NULL,rat=NULL) standardGeneric('spatialData'))
setMethod('spatialData', signature(x="list"), function(x,sheet,iterations,timesteps,nameFilters,rat) {
  # x= myResult; sheet="STSim_InitialConditionsSpatial";iterations=NULL;timesteps = NULL;rat=NULL
  if(class(x[[1]])!="Scenario"){
    stop("Expecting a Scenario object or list of scenario objects.")
  }
  for(i in 1:length(x)){
    #i=1
    cScn = x[[i]]
    cOut = spatialData(cScn,sheet,iterations,timesteps,nameFilters,rat)
    
    if(i == 1){out=cOut}else{out=raster::stack(out,cOut)}
  }
  return(out)
})

setMethod('spatialData', signature(x="Scenario"), function(x,sheet,iterations,timesteps,nameFilters,rat) {
  # x= myResult[[2]]; sheet="STSim_OutputSpatialState";iterations=NULL;timesteps = NULL;rat=NULL;nameFilters=NULL
  
  cSheets = .datasheets(x)
  if(!is.element(sheet,cSheets$name)){
    cSheets = .datasheets(x,refresh=T)
  }
  #cSheets=subset(cSheets,isSpatial)
  #if(!is.element(sheet,cSheets$name)){
  #  stop(sheet," is not a spatial data sheet.")
  #}
  
  #TO DO: make sure datasheet is spatial after opening
  cMeta = .datasheet(x,name=sheet,optional=T)
  
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
    cMeta$outName = paste0(sheet,".Scn",.scenarioId(x),".",gsub(".tif","",basename(cMeta$Filename),fixed=T))
    #cMeta$Filename = paste0(.filepath(x),".input/Scenario-",.scenarioId(x),"/",sheet,"/",cMeta$Filename)
    
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
        addPath = paste0(.filepath(x),".output/Scenario-",.scenarioId(x),"/",sheet,"/",cRow$Filename)
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

