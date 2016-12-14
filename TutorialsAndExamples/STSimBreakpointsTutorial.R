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

datasheets(myScenario)$name

myBreakpointFunction<-function(x,iteration,timestep){
  #x=scenario(myProject,id=6);iteration=2;timestep=3
  #The first argument of a breakpoint function is a SyncroSim results Scenario.
  #We can pull/push info from the Scenario database in the usual manner.
  myState = spatialData(x,sheet="STSim_InitialConditionsSpatial",
                          iterations=iteration,timesteps = timestep)[[1]]
  print(paste0("Iteration:",iteration," Timestep:",timestep," Composition:",paste(freq(myState)[,"count"],collapse=",")))
  #TO DO: modify state, replace in the database, and request reload by SyncroSim

}

#NOTE: User is responsible for ensuring that queries make sense given breakpoints.
#For example - output will be empty before iteration.

?setBreakpoint

myScenario = setBreakpoint(myScenario,"bi","stsim:core-transformer",c(1,2),myBreakpointFunction)
breakpoints(myScenario)

#TO DO: check target is valid
#DISCUSS: Should we store breakpoint information in the database? For the time being I have put it in the Scenario object.
#NOTE: breakpoints and breakpoint functions are not copied when a new scenario is created from an old one.

# devtools::document();devtools::load_all()

myResult = run(myScenario,jobs=1) #run handles breakpoints automatically



