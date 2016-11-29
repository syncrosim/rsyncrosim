# **********************************************************
# STSimSpatialTutorial.R
# Getting started with ST-Sim spatial models using rsyncrosim
# http://syncrosim.com/index.php?title=Getting_Started#Spatial_models_in_ST-Sim:_getting_from_non-spatial_to_spatial
# **********************************************************
# Author Josie Hughes, ApexRMS
# Date 2016.11.24
# **********************************************************
# devtools::document();devtools::load_all()
# devtools::test()

#*************************************
# View results from an old library (without multibanding)
myLibrary = ssimLibrary(name="C:/Temp/ST-Sim Spatial Tutorial/ST-Sim Spatial Tutorial.ssim",forceUpdate=T)
datasheets(myLibrary)

myProject = project(myLibrary)

scenarios(myProject,names=T)
myResult = scenario(myProject,id=6)
subset(datasheets(myResult),!isSpatial)$name


rat = datasheet(myResult,name="STSim_StateClass",optional=T)
#NOTE: special knowledge here of which lookup to use for legend
#QUESTION: How to interpret rat$Color?

# devtools::document();devtools::load_all()
myRasters = spatialData(myResult,sheet="STSim_OutputSpatialState",
                        iterations=seq(1,5),timesteps = seq(0,10,by=2),rat=rat)
names(myRasters)
#NOTE: multiband(x,action=rebuild) will be applied if user asks for spatialOutput() and the relevant datasheet is empty.
#TO DO: export isSpatial from SyncroSim.
#TO DO: multiband() and spatialData() for lists of Scenarios.
#DO TO: trim conflict between rsyncrosim and raster
#TO DO: also make this work for spatial inputs
#TO DO: consider options for storing raster metadata
#TO DO: get filepaths from SyncroSim.
#TO DO: unit tests and elsewhere tests. Use A176 instance for testing.

#install.packages("rasterVis")
library(raster);library(rasterVis)
#DISCUSS: dependency on raster/rdgal: only spatialData() depends on these packages. Could suggest, and complain when the function is called if the packages are missing.
#NOTE: myRasters is a normal RasterStack object.

viewRaster = myRasters[[1]]
levelplot(viewRaster,att="StateLabelXID",col.regions=colortable(viewRaster),main=viewRaster@title)

###############
# Rearrange spatial outputs in a result scenario
#set spatial options
sheetName = "STime_Options"; mySheet = datasheet(myLibrary,name=sheetName)
levels(mySheet$MultibandGroupingInternal)
mySheet[1,"MultibandGroupingInternal"]="Multiband (iterations combined)"
loadDatasheets(myProject,mySheet,name=sheetName)

#apply multiband before querying results - even if MultibandGroupingInternal="Single band"
multiband(myResult,action=rebuild)



