# source("installRSyncroSim.R") # Install the most current version of rsyncrosim. See Readme-Development.txt for details.
library(raster);library(rasterVis)

# **********************************************************
# STSimBreakpointsTutorial.R
# Getting started with breakpoints using rsyncrosim
# **********************************************************
# Author Josie Hughes, ApexRMS
# Date 2016.12.07
# **********************************************************

libRoot = "C:/Temp"

libName = "ST-Sim Spatial Tutorial"
libPath = paste0(libRoot,"/",libName,"/",libName,".ssim")
#download library if necessary.
if(!file.exists(libPath)){
  zipPath = paste0(libRoot,"/",libName,".zip")
  if(!file.exists(zipPath)){
    libURL = "http://www.apexrms.com/downloads/syncrosim/ST-Sim%20Tutorial.zip"
    download.file(libURL, zipPath)
  }
  unzip(zipPath,exdir=paste0(libRoot,"/",libName),overwrite=T,unzip = "unzip")
}

#*************************************
# Create a new scenario with breakpoints
myLibrary = ssimLibrary(name=libPath,forceUpdate=T)
myProject = project(myLibrary)
scenarios(myProject,names=T)

if(!is.element("breakpoint test",scenarios(myProject,names=T)$name)){
  myScenario = scenario(myProject,name="breakpoint test",sourceScenario = 5,author="Josie Hughes")
}else{
  myScenario = scenario(myProject,name="breakpoint test")
}

#datasheets(myScenario)$name
sheetName = "STSim_RunControl"; mySheet = datasheet(myScenario,name=sheetName,empty=F)
mySheet[1,"MaximumIteration"] = 2
mySheet[1,"MaximumTimestep"] = 3
loadDatasheets(myScenario,mySheet,name=sheetName)

sheetName = "STSim_Transition"; mySheet = datasheet(myScenario,name=sheetName,empty=F,optional=T)
mySheet$Probability[mySheet$TransitionTypeID=="Fire"]=0.9
loadDatasheets(myScenario,mySheet,name=sheetName)

# Do a comparison run without breakpoints
# myComparison = run(myScenario,jobs=1)

#TO DO: backup library before using breakpoints - wierd things can happen when breakpoint runs are interrupted.

# Write a breakpoint funtion
# The first argument of a breakpoint function is a SyncroSim results Scenario.
# source("installRSyncroSim.R") # Install the most current version of rsyncrosim. See Readme-Development.txt for details.
myBreakpointFunction<-function(x,iteration,timestep){
  #x=myComparison[[1]];iteration=2;timestep=3
  #library(raster)

  print('Breakpoint Hit')
  print(paste0('Scenario ID: ',id(x)))
  print(paste0('Iteration: ',iteration))
  print(paste0('Timestep: ',timestep))
  print("")
  # We can pull info from the Scenario database in the usual manner.

  # Use initial conditions as a base map for TransitionSpatialMultipliers
  myState = spatialData(x,sheet="STSim_InitialConditionsSpatial")[[1]]

  myMultipliers=myState
  sel =data.frame(id = seq(0,dim(myState)[1]))
  sel$step = floor((sel$id)/7)
  myMultipliers[subset(sel,is.element(step,seq(0,timestep)*2))$id,subset(sel,is.element(step,seq(0,iteration-1)*2))$id]=0

  # datasheets(x,scope="project")$name[grepl("Spatial",datasheets(x,scope="project")$name)]
  sheetName = "STSim_TransitionSpatialMultiplier"
  #myMetadata = datasheet(x,sheetName,optional=T) #This fails in parallel processing???
  myMetadata=data.frame(Iteration=iteration,Timestep=timestep,
                              TransitionGroupID="Fire",TransitionMultiplierTypeID="Temporal",
                              MultiplierFileName = paste0(sheetName,".Scn",id(x),".It",iteration,".Ts",timestep,".tif"),
                        stringsAsFactors=F)
  myMetadata$RasterLayerName = names(myMultipliers)
  myMetadata$SheetName = sheetName

  #debug loadSpatialData
  metadata=myMetadata
  data=myMultipliers
  outDir = paste0(filepath(x),'.temp/Data')
  dir.create(outDir, showWarnings = FALSE,recursive=T)
    i =1
    cRow = metadata[i,]
    cRow$SheetName=NULL
    cDat = data[[cRow$RasterLayerName]]
    cRow$RasterLayerName=NULL

    cFileCol = names(cRow)[grepl("FileName",names(cRow))]

    cRow[[cFileCol]] = basename(cRow[[cFileCol]])
    cRow[[cFileCol]] = paste0(outDir,"/",cRow[[cFileCol]])

    raster::writeRaster(cDat,cRow[[cFileCol]],overwrite=T)
    ret=loadDatasheets(x,cRow,name=sheetName,breakpoint=T)

  #loadSpatialData(x,myMultipliers,metadata=myMetadata,breakpoint=T)

  # NOTE: loadSpatialData is incomplete - it only works for breakpoint=T, metadata!=NULL, sheetName= "STSim_TransitionSpatialMultiplier"
  # NOTE: breakpoint=T. Writes csv and tif to expected temporary data directory. Does not load into database.
  # NOTE: If breakpoint = T append to existing sheet.
  # NOTE: User is responsible for ensuring that queries make sense given breakpoints.
  # NOTE: I have put the 'data-ready' call in onBreakpointHit(), rather than in the callback function - users can't muck it up there.

  # Non-spatial example
  #sheetName = "STSim_TransitionMultiplierValue"
  #mySheet = datasheet(x,sheetName,optional=T,empty=T)
  #addRows(mySheet)=data.frame(Iteration=iteration,Timestep=timestep,
  #                            TransitionGroupID="Fire",Amount=iteration*timestep+1.5)
  #mySheet=unique(mySheet)
  #loadDatasheets(x,mySheet,name=sheetName,breakpoint=T)
}

# Test breakpoint function before proceeding. If it doesn't work here, it definitely won't work later.
#myBreakpointFunction(x=myComparison[[1]],iteration=2,timestep=3)

?setBreakpoint

# Set a breakpoint in the scenario
myScenario = setBreakpoint(myScenario,"bt","stsim:core-transformer",c(1,2),myBreakpointFunction)
#breakpoints(myScenario)

# TO DO: check target is valid
# DISCUSS: Should we store breakpoint information in the database? For the time being I have put it in the Scenario object.
# NOTE: breakpoints and breakpoint functions are not copied when a new scenario is created from an old one.

myResult = run(myScenario,jobs=2) #run handles breakpoints automatically
# DISCUSS: Communication failures can stall rather than returning helpful messages. I am reluctant to put a time limit on the socket connection because simulations can take a long time. But let me know if this is a problem that needs solving.
# NOTE: Fewer helpful messages are returned during parallel processing. Use jobs=1 for debugging.
# TO DO: Method for cleanup/recovery when server is left running.

# Check what happened
datasheet(myResult,"STSim_TransitionSpatialMultiplier",optional=T) #datasheet was updated

# See multipliers
rat = data.frame(ID=c(1,0))
rat$isIn = as.logical(rat$ID)
rat$Color = c("red","wheat")
myMultipliers = spatialData(myResult,"STSim_TransitionSpatialMultiplier",rat=rat)
filename=paste0(dirname(filepath(myResult[[1]])),"/TransitionMultipliers.Scn",id(myResult[[1]]),".pdf")
pdf(filename)
view=myMultipliers;names(view)=gsub("STSim_TransitionSpatialMultiplier.","",names(view),fixed=T)
levelplot(view,att="isIn",col.regions=colortable(view[[1]]))
dev.off()

# subset(datasheets(myResult[[1]]),grepl("STSim_",name)&isOutput)$name

# Check transitions - there should be less fire in iteration 2, timestep 3
myTransitions = spatialData(myResult,"STSim_OutputSpatialTransition",rat=rat,nameFilters=c("Fire"))
for(i in 1:length(names(myTransitions))){
  #i= 1
  cName = names(myTransitions)[i]
  cFreq = freq(myTransitions[[cName]])
  cSplit = strsplit(cName,".",fixed=T)[[1]]
  bit = data.frame(name=cName,time = cSplit[4],iteration=cSplit[3],count=cFreq[1,'count'][[1]])
  if(i==1){counts = bit}else{counts=rbind(counts,bit)}
}
counts # Less fire for Ts3 It2?
filename=paste0(dirname(filepath(myResult[[1]])),"/Transitions.Scn",id(myResult[[1]]),".pdf")
pdf(filename)
view=myTransitions;names(view)=gsub("STSim_OutputSpatialTransition.","",names(view),fixed=T)
levelplot(view,att="isIn",col.regions=colortable(view[[1]]))
dev.off()

# Run again with parallel processing
# Remember - must install properly from github and load libarary to test parallel
myResult = run(myScenario,jobs=2)
# NOTE: no helpful messages with parallel processing

#Check what happened


###########
# TO DO:
# - speed up scenario construction when called from onBreakpointHit()
# - better method for finding EOL in remoteCall

