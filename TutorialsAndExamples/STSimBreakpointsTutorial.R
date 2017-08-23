# **********************************************************
# STSimBreakpointsTutorial.R
# Getting started with breakpoints using rsyncrosim
# **********************************************************
# Author Josie Hughes, ApexRMS
# Last modified 2017.08.22
# Requires: devtools::install_github("ApexRMS/dev.rsyncrosim",ref="with.breakpoints",auth_token="")
# **********************************************************

# source("installRSyncroSim.R") # Install the most current version of rsyncrosim. See Readme-Development.txt for details.
library(raster);library(rasterVis)

sessionPath = filepath(session())#"c:/gitprojects/syncrosim/_deploy_/current" #Note default session won't work until we have a real release of SyncroSim v2

#get a local copy of the package demonstration library
unzip(system.file("extdata", "DemonstrationLibrary_ssim_backup.zip", package = "rsyncrosim"),
      exdir=getwd(),overwrite=T)

#*************************************
# Create a new scenario with breakpoints
myLibrary = ssimLibrary("Demonstration Library.ssim",session=session(sessionPath),forceUpdate=T)
project(myLibrary)
myProject = project(myLibrary,1)
scenario(myProject)

if(!is.element("breakpoint test",scenario(myProject)$name)){
  myScenario = scenario(myProject,scenario="breakpoint test",sourceScenario = "No Harvest")
}else{
  myScenario = scenario(myProject,scenario="breakpoint test")
}

#datasheet(myScenario)$name
sheetName = "STSim_RunControl"; mySheet = datasheet(myScenario,name=sheetName,empty=F)
mySheet[1,"MaximumIteration"] = 2
mySheet[1,"MaximumTimestep"] = 3
saveDatasheet(myScenario,mySheet,name=sheetName)

sheetName = "STSim_Transition"; mySheet = datasheet(myScenario,name=sheetName,empty=F,optional=T)
mySheet$Probability[mySheet$TransitionTypeID=="Fire"]=0.9
saveDatasheet(myScenario,mySheet,name=sheetName)

# Do a comparison run without breakpoints
myComparison = run(myScenario,jobs=1)

# TO DO: backup library before using breakpoints?

# Write a breakpoint function
# NOTE: The first argument of a breakpoint function is a SyncroSim results Scenario.
# NOTE: Within the breakpoint function, functions from base and rsyncrosim libraries are available. Use library() within the function to load any other required packages.
myBreakpointFunction<-function(x,iteration,timestep){
  #x=myComparison;iteration=2;timestep=3
  
  print('Breakpoint Hit')
  print(paste0('Scenario ID: ',scenarioId(x)))
  print(paste0('Iteration: ',iteration))
  print(paste0('Timestep: ',timestep))
  print("")
  
  # Now we can pull info from the Scenario database in the usual manner.
  
  # Generate new TransitionSpatialMultipliers, using initial conditions as a base map
  myState = datasheetRaster(x,datasheet="STSim_InitialConditionsSpatial")[[1]]
  myMultipliers=myState
  sel =data.frame(id = seq(0,dim(myState)[1]))
  sel$step = floor((sel$id)/2)
  myMultipliers[subset(sel,is.element(step,seq(0,timestep)*2))$id,subset(sel,is.element(step,seq(0,iteration-1)*2))$id]=0
  
  # Write data and metadata for the new layer.
  sheetName = "STSim_TransitionSpatialMultiplier"
  
  pathBit = paste0(filepath(x),'.temp/Data')
  dir.create(pathBit, showWarnings = FALSE,recursive=T)
  multiplierPath = paste0(pathBit,"/FireMultiplier.tif")
  
  raster::writeRaster(myMultipliers,multiplierPath,format="GTiff",overwrite=T)
  
  #str(datasheet(myScenario,sheetName,optional=T))
  
  sheetData = data.frame(Iteration=iteration,Timestep=timestep,
                         TransitionGroupID="Fire",
                         MultiplierFileName = multiplierPath,
                         stringsAsFactors=F)
  temp = saveDatasheet(x, sheetData, sheetName,breakpoint=T)
  
  # TO DO: test using fileData
  
  # NOTE: when breakpoint=T assume sheetData contains full paths, and is otherwise valid
  # NOTE: breakpoint=T. Writes csv and tif to expected temporary data directory. Does not load into database.
  # NOTE: User is responsible for ensuring that queries make sense given breakpoints.
  # NOTE: I have put the 'data-ready' call in onBreakpointHit(), rather than in the callback function - users can't muck it up there.
  
  # Non-spatial example
  #sheetName = "STSim_TransitionMultiplierValue"
  #mySheet = datasheet(x,sheetName,optional=T,empty=T)
  #mySheet=addRow(mySheet,data.frame(Iteration=iteration,Timestep=timestep,
  #                            TransitionGroupID="Fire",Amount=iteration*timestep+1.5))
  #mySheet=unique(mySheet)
  #saveDatasheet(x,mySheet,sheetName,breakpoint=T)
}

# Test breakpoint function before proceeding. If it doesn't work here, it definitely won't work later.
myBreakpointFunction(x=myComparison,iteration=2,timestep=3)

?setBreakpoint

# Set a breakpoint in the scenario
myScenario = scenario(myProject,scenario="breakpoint test")
myScenario = setBreakpoint(myScenario,"bt","stsim:runtime",c(1,2),myBreakpointFunction)

#breakpoints(myScenario)
# TO DO: check target is valid
# DISCUSS: Should we store breakpoint information in the database? For the time being I have put it in the Scenario object.
# NOTE: breakpoints and breakpoint functions are not copied when a new scenario is created from an old one.

myResult=NULL
myResult = run(myScenario,jobs=2) #run handles breakpoints automatically
# NOTE: To run with breakpoints the first argument to run() must be a single scenario object.
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
myTransitions = datasheetRaster(myResult,"STSim_OutputSpatialTransition",subset=expression(grepl("Fire",TransitionGroupID,fixed=T)))
names(myTransitions)

for(i in 1:length(names(myTransitions))){
  #i= 1
  cName = names(myTransitions)[i]
  cFreq = freq(myTransitions[[cName]])
  cSplit = strsplit(cName,".",fixed=T)[[1]]
  bit = data.frame(name=cName,time = cSplit[3],iteration=cSplit[2],count=cFreq[1,'count'][[1]])
  if(i==1){counts = bit}else{counts=rbind(counts,bit)}
}
counts # Less fire for Ts3 It2?
filename=paste0(dirname(filepath(myResult)),"/Transitions.Scn",scenarioId(myResult),".pdf")
pdf(filename)
plot(myTransitions)
dev.off()

# NOTE: "STSim_TransitionSpatialMultiplier" is not updated properly during parallel processing. But effects can be seen in transitions.
datasheet(myResult,"STSim_TransitionSpatialMultiplier",optional=T) #datasheet was updated
myMultiplier = datasheetRaster(myResult,"STSim_TransitionSpatialMultiplier")
filename=paste0(dirname(filepath(myResult)),"/TransitionMultipliers.Scn",scenarioId(myResult),".pdf")
pdf(filename)
plot(myMultiplier)
dev.off()

