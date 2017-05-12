#library(rsyncrosim)
# source("installRSyncroSim.R") # Install the most current version of rsyncrosim. See Readme-Development.txt for details.

library(raster);library(rasterVis);library(rsyncrosim)
libRoot = "C:/Temp/ST-Sim Spatial Tutorial2"
libName = "ST-Sim Spatial Tutorial"
libPath = paste0(libRoot,"/",libName,".ssim")
myLibrary = ssimLibrary(name=libPath,forceUpdate=T)
myProject = project(myLibrary,project="a project")
scenarios(myProject,names=T)

myBreakpointFunction<-function(x,iteration,timestep){
  print('Breakpoint Hit')
  print(paste0('Scenario ID: ',id(x)))
  print(paste0('Iteration: ',iteration))
  print(paste0('Timestep: ',timestep))
  print("")
}

deleteScenarios(myLibrary, scenario=c("breakpoint test copy"),force=T) #start fresh
myScenario = scenario(myProject,name="breakpoint test copy",sourceScenario = "breakpoint test")
myTest = run(myScenario,jobs=2)

myScenario = setBreakpoint(myScenario,"bt","stsim:core-transformer",c(1,2),myBreakpointFunction)
myResult = run(myScenario,jobs=2) #Fails if there are dependencies

myFlatScenario =scenario(myProject,name="breakpoint flat") # this is a "paste and merge dependencies" copy of "breakpoint test" scenario
myFlatScenario = setBreakpoint(myFlatScenario,"bt","stsim:core-transformer",c(1,2),myBreakpointFunction)

