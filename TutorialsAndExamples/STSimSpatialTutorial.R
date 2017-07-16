# source("installRSyncroSim.R") # Install the most current version of rsyncrosim. See Readme-Development.txt for details.
#library(rsyncrosim)

# **********************************************************
# STSimSpatialTutorial.R
# Getting started with ST-Sim spatial models using rsyncrosim - view spatial inputs and outputs
# http://syncrosim.com/index.php?title=Getting_Started#Spatial_models_in_ST-Sim:_getting_from_non-spatial_to_spatial
# **********************************************************
# Author Josie Hughes, ApexRMS
# Date 2016.12.06
# **********************************************************

#install.packages("rasterVis")
library(raster);library(rasterVis)
sessionPath = "c:/gitprojects/syncrosim/_deploy_/current" #Note default session won't work until we have a real release of SyncroSim v2
libRoot = "C:/Temp"
libName = "ST-Sim Spatial Tutorial"
libPath = paste0(libRoot,"/",libName,"/",libName,".ssim")
delete(libPath,force=T)# start fresh
#download library if necessary.
if(!file.exists(libPath)){
  zipPath = paste0(libRoot,"/",libName,".zip")
  if(!file.exists(zipPath)){
    libURL = "http://www.apexrms.com//wp-content//uploads//ST-Sim-Spatial-Tutorial.zip"
    download.file(libURL, zipPath)
  }
  unzip(zipPath,exdir=paste0(libRoot,"/",libName),overwrite=T,unzip = "unzip")
}

#*************************************
# View  "STSim_OutputSpatialState" results
myLibrary = ssimLibrary(name=libPath,session=session(sessionPath),forceUpdate=T)

project(myLibrary)
myProject = project(myLibrary,project=1)
scenario(myProject)
# source("installRSyncroSim.R") # Install the most current version of rsyncrosim. See Readme-Development.txt for details.

run(myProject,5,summary=T)

resultScns = scenario(myProject,results=T)
myResult = scenario(myProject,scenario=tail(resultScns,n=2)$scenarioId)

#***********************************
# Define function for plotting categorical maps. 
# Plot a RasterLayer with raster attributes set by spatialData() or ssimRatify().
# This is a wrapper around the levelplot() function of the rasterVis package.
# @param raster A RasterLayer with a raster attribute table set by spatialData() or ssimRatify().
# @param attribute character string. The attribute to be plotted. This must be a column name in the raster attribute table.
# @param ... additional arguments passed to rasterVis::levelplot.
ssimLevelplot<-function(raster,attribute,...){
  myLevels = raster::levels(raster)[[1]]
  myCols = unique(subset(myLevels,select=c(attribute,"hexColor")));myCols=myCols[order(myCols[,1]),]
  print(rasterVis::levelplot(raster,att=attribute,at=myCols[,attribute],col.regions=myCols$hexColor,par.settings=myCols,...))
}

#*************************************
# View state class output
# Add an (optional) raster attribute table. This is dataframe with ID, (optional) Color, and descriptor columns.
# In this example, we load StateClass attributes from the library, then override the Colors.
rat = datasheet(myResult[[1]],name="STSim_StateClass",optional=T)
rat$Color#We could use Colors from the library. Or override.
rat$Color = c("darkgreen","brown","wheat")
# The (optional) Color column of a rat table should have one of these formats:
#   alpha,R,G,B: 4 numbers representing alpha, red, green, and blue, separated by commas, and scaled between 0 and 255. See rgb() for details.
#   R colour names: See colors() for options.
#   hexadecimal colors: As returned by R functions such as rainbow(), heat.colors(), terrain.colors(), topo.colors(), gray(), etc.

checkSheet = datasheet(myResult,name="STSim_OutputSpatialState")
# source("installRSyncroSim.R") # Install the most current version of rsyncrosim. See Readme-Development.txt for details.
myRasters = datasheetRaster(myResult,datasheet="STSim_OutputSpatialState",
                        iteration=seq(1),timestep = seq(0,10,by=5),rat=rat)
names(myRasters)

str(myRasters[[1]])
levels(myRasters[[1]]) #attributes from the optional rat table. Color name were converted to hexadecimal colors in hexColor column.
#NOTE: myRasters is a RasterStack object. See raster package documentation for details.
#NOTE: loading is faster if all sheets are contained in a single multiband file. See below for example.

#plot iteration 1 timestep 0
# source("installRSyncroSim.R") # Install the most current version of rsyncrosim. See Readme-Development.txt for details.
ssimLevelplot(myRasters[[1]],attribute="StateLabelXID",main=names(myRasters)[1]) 

#Change to automatically selected colors and save plot to pdf.
newRat = levels(myRasters[[1]])[[1]]
newRat$Color = brewer.pal(n = nrow(newRat), name = "Dark2")
ssimRatify(myRasters[[1]]) = newRat #change the raster attributes

filename=paste0(dirname(filepath(myResult[[1]])),"/XIDMap.pdf")
pdf(filename)
ssimLevelplot(myRasters[[1]],attribute="StateLabelXID")
dev.off()

#*************************************
# View spatial inputs
check = datasheet(myResult[[1]],"STSim_OutputSpatialTransition")
str(check)
myTransitionGroup = datasheetRaster(myLibrary,scenario=as.numeric(names(myResult)),datasheet="STSim_OutputSpatialTransition",timestep=1,iteration=1,subset=expression(TransitionGroupID=="Fire"))
#slower with scenario argument than list of scenario objects.
names(myTransitionGroup)

check = datasheet(myResult[[1]],"STSim_InitialConditionsSpatial")
str(check)
mySpatialInputs = datasheetRaster(myResult,datasheet="STSim_InitialConditionsSpatial",column="AgeFileName")
names(mySpatialInputs)
age0=mySpatialInputs[["scn7.age.it0.ts0"]]

#see all ages
plot(age0,main=age0@title)

#Build raster attribute table to view young only
rat = data.frame(ID=unique(age0))
rat$isYoung[rat$ID<36]="young" #check young forest definition
rat$isYoung[rat$ID>=36]="not young" #check young forest definition
rat$Color = "wheat"; rat$Color[rat$isYoung=="young"]="darkgreen"
ssimRatify(age0) = rat

filename=paste0(dirname(filepath(myResult[[1]])),"/youngMap.pdf")
pdf(filename)
ssimLevelplot(age0,attribute="isYoung")
dev.off()

#NOTE: multiband(x,action=rebuild) will be applied if user asks for spatialData() and the relevant datasheet is empty.
#TO DO: check non-stsim spatial inputs
#TO DO: check writing colors back to SyncroSim
#NOTE: special knowledge of lookups to use for legends

##################
#Set spatial inputs in a new library.
if(is.element("NewScn",scenario(myProject)$name)){
  delete(myProject,scenario="NewScn",force=T)
}
if(is.element("Another New Scenario",scenario(myProject)$name)){
  delete(myProject,scenario="Another New Scenario",force=T)
}
newScenario = scenario(myProject,scenario="NewScn")

inRasters=stack(datasheetRaster(myResult[[1]],datasheet="STSim_InitialConditionsSpatial",column="AgeFileName"),
                datasheetRaster(myResult[[1]],datasheet="STSim_InitialConditionsSpatial",column="StateClassFileName"),
                datasheetRaster(myResult[[1]],datasheet="STSim_InitialConditionsSpatial",column="StratumFileName"))
names(inRasters)=gsub(".it0.ts0","",names(inRasters),fixed=T)
#saveDatasheet is expecting a named raster stack or named list.

#inSheet must conform to format expectations just like any other datasheet.
#Additionally, each element of names(inRasters) must be entered in appropriately in inSheet.
sheetName = "STSim_InitialConditionsSpatial"
inSheet = datasheet(newScenario,name=sheetName)
inSheet[1,"StratumFileName"]=names(inRasters)[3]
inSheet[1,"StateClassFileName"]=names(inRasters)[2]
inSheet[1,"AgeFileName"]=names(inRasters)[1]
saveDatasheet(newScenario,data=inSheet,name=sheetName,fileData=inRasters)
datasheet(newScenario,name="STSim_InitialConditionsSpatialProperties")
#spatial metadata is set automatically when rasters are imported.

#Change the extent of the input maps
inExtent = extent(inRasters)
outExtent = inExtent
outExtent@xmax = 500;outExtent@ymax=500
newRasters=crop(inRasters,outExtent)
dim(newRasters)
anotherScenario = scenario(myProject,"Another New Scenario")
saveDatasheet(anotherScenario,data=inSheet,name=sheetName,fileData=newRasters)
datasheet(anotherScenario,name="STSim_InitialConditionsSpatialProperties")

###############
# Rearrange spatial outputs in a result scenario
#set spatial options
sheetName = "STime_Options"; mySheet = datasheet(myLibrary,name=sheetName)
levels(mySheet$MultibandGroupingInternal)
mySheet[1,"MultibandGroupingInternal"]="Multiband (iterations and timesteps combined)"
saveDatasheet(myProject,mySheet,name=sheetName)

#Combining all spatial results into one multiband file will speed up loading.
multiband(myResult[[1]],action="apply")

