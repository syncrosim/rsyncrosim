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

libRoot = "C:/Temp"
libName = "ST-Sim Spatial Tutorial"
libPath = paste0(libRoot,"/",libName,"/",libName,".ssim")
delete(libPath,force=T)
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

project(myLibrary)
myProject = project(myLibrary,project=1)
scenario(myProject)
# source("installRSyncroSim.R") # Install the most current version of rsyncrosim. See Readme-Development.txt for details.

run(myProject,5,summary=T)

resultScns = scenario(myProject,results=T)
myResult = scenario(myProject,scenario=tail(resultScns,n=2)$id)

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

# source("installRSyncroSim.R") # Install the most current version of rsyncrosim. See Readme-Development.txt for details.

checkSheet = datasheet(myResult,name="STSim_OutputSpatialState")
myRasters = spatialData(myResult,sheet="STSim_OutputSpatialState",
                        iterations=seq(1),timesteps = seq(0,10,by=5),rat=rat)
names(myRasters)

str(myRasters[[1]])
#NOTE: myRasters is a RasterStack object. See raster package documentation for details.
#NOTE: loading is faster if all sheets are contained in a single multiband file. See below for example.

#plot iteration 1 timestep 0
?ratify
?scenario
# source("installRSyncroSim.R") # Install the most current version of rsyncrosim. See Readme-Development.txt for details.

levels(myRasters[[1]])
levelplotCategorical(myRasters[[1]],attribute="StateLabelXID")
#This is a wrapper for the levelplot() function of the rasterVis package:

  view = myRasters[[1]];attribute="StateLabelXID"
  levels(view)
  myCols = unique(subset(levels(view)[[1]],select=c(attribute,"hexColor")));myCols=myCols[order(myCols[,1]),]
  levelplot(view,att=attribute,at=myCols$Name,col.regions=myCols$hexColor,par.settings=myCols,main=view@title)

#Is this function sufficiently useful? Otherwise rasterVis is not a recommended or required package. 

#Change to automatically selected colors and save plot to pdf.
newRat = levels(myRasters[[1]])[[1]]
newRat$Color = brewer.pal(n = nrow(newRat), name = "Dark2")
rasterAttributes(myRasters[[1]]) = newRat

?'rasterAttributes<-'

filename=paste0(dirname(filepath(myResult[[1]])),"/XIDMap.pdf")
pdf(filename)
levelplotCategorical(myRasters[[1]],attribute="StateLabelXID")
dev.off()

#*************************************
# View spatial inputs
datasheet(myResult[[1]])$name

check = datasheet(myResult[[1]],"STSim_OutputSpatialTransition")
str(check)
#TO DO: helper for pulling particular transition groups, etc? later.
mySpatialInputs = spatialData(myResult,sheet="STSim_InitialConditionsSpatial")
names(mySpatialInputs)
age0=mySpatialInputs[["STSim_InitialConditionsSpatial.Scn7.It0000.Ts0000.age"]]
#TO DO:show example of pulling sheet without writing full name

#see all ages
plot(age0,main=age0@title)

#Build raster attribute table to view young only
rat = data.frame(ID=unique(age0))
rat$isYoung[rat$ID<36]="young" #check young forest definition
rat$isYoung[rat$ID>=36]="not young" #check young forest definition
rat$Color = "wheat"; rat$Color[rat$isYoung=="young"]="darkgreen"
rasterAttributes(age0) = rat

filename=paste0(dirname(filepath(myResult[[1]])),"/youngMap.pdf")
pdf(filename)
levelplotCategorical(age0,attribute="isYoung")
dev.off()

?`rasterAttributes<-`

#NOTE: multiband(x,action=rebuild) will be applied if user asks for spatialData() and the relevant datasheet is empty.
#TO DO: check non-stsim spatial inputs
#TO DO: spatial inputs
#TO DO: write colors back to SyncroSim
#TO DO: make ?rasterAttributes work
#TO DO: get full paths from SyncroSim - special handling of output files that are spatial?
#NOTE: special knowledge of lookups to use for legends
#DISCUSS: dependency on raster/rdgal: only spatialData() and rasterAttributes() depend on these packages. Could suggest, and complain when the function is called if the packages are missing.
#DISCUSS: options for storing raster metadata

##################
#Set spatial inputs in a new library.
if(is.element("NewScn",scenario(myProject)$name)){
  delete(myProject,scenario="NewScn",force=T)
}
newScenario = scenario(myProject,scenario="NewScn")

metadata = data.frame(StratumFileName="It0000-Ts0000-str.tif",StateClassFileName="It0000-Ts0000-sc.tif",AgeFileName="It0000-Ts0000-age.tif")
metadata$SheetName="STSim_InitialConditionsSpatial"
data=stack(mySpatialInputs[["STSim_InitialConditionsSpatial.Scn7.It0000.Ts0000.age"]],
          mySpatialInputs[["STSim_InitialConditionsSpatial.Scn7.It0000.Ts0000.sc"]],
          mySpatialInputs[["STSim_InitialConditionsSpatial.Scn7.It0000.Ts0000.str"]])
names(data)=gsub("STSim_InitialConditionsSpatial.Scn7.","",names(data),fixed=T)
names(data)=paste0(names(data),".tif")
# source("installRSyncroSim.R") # Install the most current version of rsyncrosim. See Readme-Development.txt for details.

spatialProperties = loadSpatialData(newScenario,data,metadata)

#Set spatial metadata - this is specific to stsim initial conditions
#If using a different model the user is responsible for ensuring that spatialProperties data returned by loadSpatialData() is sufficient and appropriate.
sheetName = "STSim_InitialConditionsSpatialProperties"; mySheet = datasheet(newScenario,name=sheetName,optional=F,empty=T)
spatialProperties$CellArea=spatialProperties$CellSize^2/10000
mySheet=addRows(mySheet,spatialProperties)
saveDatasheet(newScenario,mySheet,name=sheetName)

#NOTE: This does not work. Throws exception when I try to open in GUI. Help?

###############
# Rearrange spatial outputs in a result scenario
#set spatial options
sheetName = "STime_Options"; mySheet = datasheet(myLibrary,name=sheetName)
levels(mySheet$MultibandGroupingInternal)
mySheet[1,"MultibandGroupingInternal"]="Multiband (iterations and timesteps combined)"
saveDatasheet(myProject,mySheet,name=sheetName)
?multiband
#Combining all spatial results into one multiband file will speed up loading.
multiband(myResult,action="apply")

