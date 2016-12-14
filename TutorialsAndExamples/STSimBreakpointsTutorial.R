# source("installRSyncroSim.R") # Install the most current version of rsyncrosim. See Readme-Development.txt for details.
# library(rsyncrosim)
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

# devtools::document();devtools::load_all()
myBreakpointFunction<-function(x,iteration,timestep){
  #x=scenario(myProject,id=8);iteration=2;timestep=1
  #The first argument of a breakpoint function is a SyncroSim results Scenario.

  print('Breakpoint Hit')
  print(paste0('Scenario ID: ',id(x)))
  print(paste0('Iteration: ',iteration))
  print(paste0('Timestep: ',timestep))
  print(paste0('Out Dir: ',filepath(x),".temp/Data"))
  print("")

  #We can pull info from the Scenario database in the usual manner.
  sheetName = "STSim_TransitionMultiplierValue"
  mySheet = datasheet(x,sheetName,optional=T,empty=T)
  addRows(mySheet)=data.frame(Iteration=iteration,Timestep=timestep,
                              TransitionGroupID="Fire",Amount=iteration*timestep+1.5)
  mySheet=unique(mySheet)
  loadDatasheets(x,mySheet,name=sheetName,breakpoint=T)
  # NOTE: breakpoint=T. Writes csv to expected temporary data directory. Does not load into database.
  # NOTE: If breakpoint = T append to existing sheet.

  # NOTE: User is responsible for ensuring that queries make sense given breakpoints.
  # For example - output will be empty before iteration.

  # NOTE: I have put the 'data-ready' call in onBreakpointHit(), rather than in the callback function - users can't muck it up there.

  # TO DO: Example modifying spatial state.
  #myState = spatialData(x,sheet="STSim_InitialConditionsSpatial",
  #                        iterations=iteration,timesteps = timestep)[[1]]
  #print(paste0("Iteration:",iteration," Timestep:",timestep," Composition:",paste(freq(myState)[,"count"],collapse=",")))
}

?setBreakpoint

myScenario = setBreakpoint(myScenario,"bt","stsim:core-transformer",c(1,2),myBreakpointFunction)
#breakpoints(myScenario)

#TO DO: check target is valid
#DISCUSS: Should we store breakpoint information in the database? For the time being I have put it in the Scenario object.
#NOTE: breakpoints and breakpoint functions are not copied when a new scenario is created from an old one.

# devtools::document();devtools::load_all()

myResult = run(myScenario,jobs=1) #run handles breakpoints automatically
# DISCUSS: communication failures can stall rather than returning helpful messages. Do I need to put more time into this?
# NOTE: Fewer helpful messages are returned for parallel processing. Use jobs=1 for debugging.

# Check what happened
multipliers = datasheet(myResult,"STSim_TransitionMultiplierValue",optional=T)
subset(multipliers,select=c(Iteration,Timestep,Amount))
# QUESTION: How to confirm the datasheet had appropriate effects on the simulation?

# Run again with parallel processing
# Remember - must install properly from github and load libarary to test parallel
#myResult = run(myScenario,jobs=2)
# NOTE: no helpful messages with parallel processing

#Check what happened
#multipliers = datasheet(myResult,"STSim_TransitionMultiplierValue",optional=T)
#subset(multipliers,select=c(Iteration,Timestep,Amount))

###########
# TO DO:
# - speed up scenario construction when called from onBreakpointHit()
# - less stupid way of finding EOL in remoteCall

