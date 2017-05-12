# **********************************************************
# STSimBreakpointsTutorial.R
# Getting started with breakpoints using rsyncrosim
# **********************************************************
# Author Josie Hughes, ApexRMS
# Last modified 2016.12.16
# **********************************************************

# source("installRSyncroSim.R") # Install the most current version of rsyncrosim. See Readme-Development.txt for details.
install.packages("rasterVis")
library(raster);library(rasterVis)

libRoot = "C:/Temp"

libName = "ST-Sim Spatial Tutorial"
libPath = paste0(libRoot,"/",libName,"/",libName,".ssim")
#download library if necessary.
if(!file.exists(libPath)){
  zipPath = paste0(libRoot,"/",libName,".zip")
  if(!file.exists(zipPath)){
    libURL = "http://www.apexrms.com/downloads/syncrosim/ST-Sim%20Spatial%20Tutorial.zip"
    download.file(libURL, zipPath)
  }
  unzip(zipPath,exdir=paste0(libRoot,"/",libName),overwrite=T,unzip = "unzip")
}
#If unzip fails (status 127) ensure that a zip program is installed. Installing RTools will solve the problem.

#*************************************
# Create a new scenario with breakpoints
myLibrary = ssimLibrary(name=libPath,forceUpdate=T)
myProject = project(myLibrary,project="a project")
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
myComparison = run(myScenario,jobs=1)

# TO DO: backup library before using breakpoints?

# Write a breakpoint function
# NOTE: The first argument of a breakpoint function is a SyncroSim results Scenario.
# NOTE: Within the breakpoint function, functions from base and rsyncrosim libraries are available. Use library() within the function to load any other required packages.
myBreakpointFunction<-function(x,iteration,timestep){
  #x=myComparison[[1]];iteration=2;timestep=3

  print('Breakpoint Hit')
  print(paste0('Scenario ID: ',id(x)))
  print(paste0('Iteration: ',iteration))
  print(paste0('Timestep: ',timestep))
  print("")

  # Now we can pull info from the Scenario database in the usual manner.

  # Generate new TransitionSpatialMultipliers, using initial conditions as a base map
  myState = spatialData(x,sheet="STSim_InitialConditionsSpatial")[[1]]
  myMultipliers=myState
  sel =data.frame(id = seq(0,dim(myState)[1]))
  sel$step = floor((sel$id)/7)
  myMultipliers[subset(sel,is.element(step,seq(0,timestep)*2))$id,subset(sel,is.element(step,seq(0,iteration-1)*2))$id]=0

  # Write metadata for the new layer.
  sheetName = "STSim_TransitionSpatialMultiplier"
  #myMetadata = datasheet(x,sheetName,optional=T) #Save time by skipping this step
  myMetadata=data.frame(Iteration=iteration,Timestep=timestep,
                              TransitionGroupID="Fire",TransitionMultiplierTypeID="Temporal",
                              MultiplierFileName = paste0(sheetName,".Scn",id(x),".It",iteration,".Ts",timestep,".tif"),
                        stringsAsFactors=F)
  myMetadata$RasterLayerName = names(myMultipliers)
  myMetadata$SheetName = sheetName

  # Load the new layer and associated metadata.
  temp = loadSpatialData(x,myMultipliers,metadata=myMetadata,breakpoint=T,check=F)

  # NOTE: set check=F to speed calculations. Assume metadata is valid
  # NOTE: loadSpatialData is incomplete - it only works for breakpoint=T, metadata!=NULL, sheetName= "STSim_TransitionSpatialMultiplier"
  # NOTE: breakpoint=T. Writes csv and tif to expected temporary data directory. Does not load into database.
  # NOTE: If breakpoint = T append to existing sheet.
  # NOTE: User is responsible for ensuring that queries make sense given breakpoints.
  # NOTE: I have put the 'data-ready' call in onBreakpointHit(), rather than in the callback function - users can't muck it up there.

  # Non-spatial example
  #sheetName = "STSim_TransitionMultiplierValue"
  #mySheet = datasheet(x,sheetName,optional=T,empty=T)
  #mySheet=addRows(mySheet,data.frame(Iteration=iteration,Timestep=timestep,
  #                            TransitionGroupID="Fire",Amount=iteration*timestep+1.5))
  #mySheet=unique(mySheet)
  #loadDatasheets(x,mySheet,name=sheetName,breakpoint=T)
}

# Test breakpoint function before proceeding. If it doesn't work here, it definitely won't work later.
myBreakpointFunction(x=myComparison[[1]],iteration=2,timestep=3)

?setBreakpoint

# Set a breakpoint in the scenario
myScenario = setBreakpoint(myScenario,"bt","stsim:core-transformer",c(1,2),myBreakpointFunction)
#breakpoints(myScenario)
# TO DO: check target is valid
# DISCUSS: Should we store breakpoint information in the database? For the time being I have put it in the Scenario object.
# NOTE: breakpoints and breakpoint functions are not copied when a new scenario is created from an old one.

myResult=NULL
myResult = run(myScenario,jobs=2) #run handles breakpoints automatically
# NOTE: If connection error during parallel processing - Try again. I am working on a fix for this bug.
# TO DO: handle connection failure - on first try only. Why? Probably timing.
# NOTE: Fewer helpful messages are returned during parallel processing. Use jobs=1 for debugging.
# NOTE: Communication failures can stall rather than returning helpful messages. I am reluctant to put a time limit on the socket connection because simulations can take a long time. But let me know if this is a problem that needs solving.
# TO DO: Use fork clusters on linux? Better memory use.
# TO DO: Figure out logging to output file during parallel processing. Not sure where the print messages are going...
# NOTE: must install properly from github and load libarary to test parallel
# TO DO: speed up scenario construction when called from onBreakpointHit()
# TO DO: better handling of EOL in remoteCall

# Check what happened
# Check transitions - there should be less fire in iteration 2, timestep 3
rat = data.frame(ID=c(1),isIn=c("Fire"),Color=c("red"))
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

# NOTE: "STSim_TransitionSpatialMultiplier" is not updated properly during parallel processing. But effects can be seen in transitions.
datasheet(myResult,"STSim_TransitionSpatialMultiplier",optional=T) #datasheet was updated
myMultipliers = spatialData(myResult,"STSim_TransitionSpatialMultiplier",rat=rat)
filename=paste0(dirname(filepath(myResult[[1]])),"/TransitionMultipliers.Scn",id(myResult[[1]]),".pdf")
pdf(filename)
view=myMultipliers;names(view)=gsub("STSim_TransitionSpatialMultiplier.","",names(view),fixed=T)
levelplot(view,att="isIn",col.regions=colortable(view[[1]]))
dev.off()

