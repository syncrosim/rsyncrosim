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

#install.packages("rasterVis")
library(raster);library(rasterVis)

#*************************************
# View  "STSim_OutputSpatialState" results
myLibrary = ssimLibrary(name="C:/Temp/ST-Sim Spatial Tutorial/ST-Sim Spatial Tutorial.ssim",forceUpdate=T)

myProject = project(myLibrary)

scenarios(myProject,names=T)
myResult = scenario(myProject,id=6)
subset(datasheets(myResult),isSpatial)$name

# Add an (optional) raster attribute table. This is dataframe with ID, (optional) Color, and descriptor columns.
# In this example, we load StateClass attributes from the library, then override the Colors.
rat = datasheet(myResult,name="STSim_StateClass",optional=T)
rat$Color#We could use Colors from the library. But 2 of these colors are transparent. So override.
rat$Color = c("darkgreen","brown","wheat")
# The (optional) Color column of a rat table should have one of these formats:
#   R,G,B,alpha: 4 numbers representing red, green, blue and alpha, separated by commas, and scaled between 0 and 255. See rgb() for details.
#   R colour names: See colors() for options.
#   hexadecimal colors: As returned by R functions such as rainbow(), heat.colors(), terrain.colors(), topo.colors(), gray(), etc.

# devtools::document();devtools::load_all()

myRasters = spatialData(myResult,sheet="STSim_OutputSpatialState",
                        iterations=seq(1),timesteps = seq(0,10,by=5),rat=rat)
names(myRasters)
#NOTE: myRasters is a RasterStack object. See raster package documentation for details.
#NOTE: loading is faster if all sheets are contained in a single multiband file. See below for example.

viewRaster = myRasters[[1]]
levelplot(viewRaster,att="StateLabelXID",col.regions=colortable(viewRaster),main=viewRaster@title)

#NOTE: multiband(x,action=rebuild) will be applied if user asks for spatialOutput() and the relevant datasheet is empty.
#TO DO: export isSpatial from SyncroSim.
#TO DO: multiband() and spatialData() for lists of Scenarios.
#TO DO: also make this work for spatial inputs
#TO DO: get filepaths from SyncroSim.
#TO DO: unit tests and elsewhere tests. Use A176 instance for testing.
#NOTE: special knowledge of lookup to use for legend
#DISCUSS: dependency on raster/rdgal: only spatialData() depends on these packages. Could suggest, and complain when the function is called if the packages are missing.
#DISCUSS: consider options for storing raster metadata

###############
# Rearrange spatial outputs in a result scenario
#set spatial options
sheetName = "STime_Options"; mySheet = datasheet(myLibrary,name=sheetName)
levels(mySheet$MultibandGroupingInternal)
mySheet[1,"MultibandGroupingInternal"]="Multiband (iterations and timesteps combined)"
loadDatasheets(myProject,mySheet,name=sheetName)

multiband(myResult,action="apply")



