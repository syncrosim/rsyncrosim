# source("installRSyncroSim.R") # Install the most current version of rsyncrosim. See Readme-Development.txt for details.
library(rsyncrosim)

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

libRoot = "C:/Temp"
libName = "ST-Sim Spatial Tutorial"
libPath = paste0(libRoot,"/",libName,"/",libName,".ssim")
#download library if necessary.
if(!file.exists(libPath)){
  zipPath = paste0(libRoot,"/",libName,".zip")
  if(!file.exists(zipPath)){
    libURL = "http://www.apexrms.com//downloads/syncrosim/ST-Sim%20Spatial%20Tutorial.zip"
    download.file(libURL, zipPath)
  }
  unzip(zipPath,exdir=paste0(libRoot,"/",libName),overwrite=T,unzip = "unzip")
}

#*************************************
# View  "STSim_OutputSpatialState" results
myLibrary = ssimLibrary(name=libPath,forceUpdate=T)

myProject = project(myLibrary)
run(myProject,5,onlyIds=T)

scenarios(myProject,names=T)
myResult = scenarios(myProject,select=c(6,7))
datasheets(myResult[[1]])$name

#*************************************
# View state class output
# Add an (optional) raster attribute table. This is dataframe with ID, (optional) Color, and descriptor columns.
# In this example, we load StateClass attributes from the library, then override the Colors.
rat = datasheet(myResult[[1]],name="STSim_StateClass",optional=T)
rat$Color#We could use Colors from the library. But 2 of these colors are transparent. So override.
rat$Color = c("darkgreen","brown","wheat")
# The (optional) Color column of a rat table should have one of these formats:
#   R,G,B,alpha: 4 numbers representing red, green, blue and alpha, separated by commas, and scaled between 0 and 255. See rgb() for details.
#   R colour names: See colors() for options.
#   hexadecimal colors: As returned by R functions such as rainbow(), heat.colors(), terrain.colors(), topo.colors(), gray(), etc.

myRasters = spatialData(myResult,sheet="STSim_OutputSpatialState",
                        iterations=seq(1),timesteps = seq(0,10,by=5),rat=rat)
names(myRasters)
#NOTE: myRasters is a RasterStack object. See raster package documentation for details.
#NOTE: loading is faster if all sheets are contained in a single multiband file. See below for example.

#plot iteration 1 timestep 0
view = myRasters[[1]]
levelplot(view,att="StateLabelXID",col.regions=colortable(view),main=view@title)

#Change to automatically selected colors and save plot to pdf.
newRat = levels(view)[[1]]
newRat$Color = brewer.pal(n = nrow(newRat), name = "Dark2")
rasterAttributes(view) = newRat

filename=paste0(dirname(filepath(myResult[[1]])),"/youngMap.pdf")
pdf(filename)
levelplot(view,att="StateLabelXID",col.regions=colortable(view),main=view@title)
dev.off()

#*************************************
# View spatial inputs
datasheets(myResult[[1]])$name

mySpatialInputs = spatialData(myResult,sheet="STSim_InitialConditionsSpatial")
names(mySpatialInputs)
age0=mySpatialInputs[["STSim_InitialConditionsSpatial.Scn6.It0000.Ts0000.age"]]

#see all ages
plot(age0,main=age0@title)

#Build raster attribute table to view young only
rat = data.frame(ID=unique(age0))
rat$isYoung[rat$ID<36]="young" #check young forest definition
rat$isYoung[rat$ID>=36]="not young" #check young forest definition
rat$Color = "wheat"; rat$Color[rat$isYoung=="young"]="darkgreen"
rasterAttributes(age0) = rat
# devtools::document();devtools::load_all()

filename=paste0(dirname(filepath(myResult[[1]])),"/youngMap.pdf")
pdf(filename)
view=age0;levelplot(view,att="isYoung",col.regions=colortable(view),main=view@title)
dev.off()

#NOTE: multiband(x,action=rebuild) will be applied if user asks for spatialOutput() and the relevant datasheet is empty.
#TO DO: check non-stsim spatial inputs
#TO DO: unit tests.
#TO DO: spatial inputs
#TO DO: write colors back to SyncroSim
#TO DO: make ?rasterAttributes work
#TO DO: get full paths from SyncroSim
#NOTE: special knowledge of lookups to use for legends
#DISCUSS: dependency on raster/rdgal: only spatialData() and rasterAttributes() depend on these packages. Could suggest, and complain when the function is called if the packages are missing.
#DISCUSS: options for storing raster metadata

###############
# Rearrange spatial outputs in a result scenario
#set spatial options
sheetName = "STime_Options"; mySheet = datasheet(myLibrary,name=sheetName)
levels(mySheet$MultibandGroupingInternal)
mySheet[1,"MultibandGroupingInternal"]="Multiband (iterations and timesteps combined)"
loadDatasheets(myProject,mySheet,name=sheetName)

#Combining all spatial results into one multiband file will speed up loading.
multiband(myResult,action="apply")

