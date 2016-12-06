# source("installRSyncroSim.R") # Install the most current version of rsyncrosim. See Readme-Development.txt for details.
#library(plyr);library(rsyncrosim);library(raster);library(rasterVis)
#library(testthat)
retDir = getwd()
#setwd("..")
#getwd()
unlink("testLibs",recursive=T)
dir.create('testLibs')
setwd("./testLibs")

# **********************************************************
# STSimSpatialTutorial.R
# Getting started with ST-Sim spatial models using rsyncrosim - view spatial inputs and outputs
# http://syncrosim.com/index.php?title=Getting_Started#Spatial_models_in_ST-Sim:_getting_from_non-spatial_to_spatial
# **********************************************************
# Author Josie Hughes, ApexRMS
# Date 2016.12.06
# **********************************************************

test_that("Test simple spatial STSim example", {
  library(raster);library(rasterVis)
  libRoot = getwd()
  libPath = paste0(libRoot,"/ST-Sim Spatial Tutorial/ST-Sim Spatial Tutorial.ssim")
  #download library if necessary.
  if(!file.exists(libPath)){
    zipPath = paste0(libRoot,"/ST-Sim Spatial Tutorial.zip")
    if(!file.exists(zipPath)){
      libURL = "http://www.apexrms.com//downloads/syncrosim/ST-Sim%20Spatial%20Tutorial.zip"
      download.file(libURL, zipPath,quiet=T)

    }
    unzip(zipPath,exdir=paste0(libRoot,"/ST-Sim Spatial Tutorial"),overwrite=T,unzip = "unzip")
  }

  #*************************************
  # View  "STSim_OutputSpatialState" results
  myLibrary = ssimLibrary(name=libPath,forceUpdate=T)

  myProject = project(myLibrary)

  if(!is.element(7,scenarios(myProject,names=T)$id)){
    temp = run(myProject,5,onlyIds=T)
  }
  myResult = scenarios(myProject,select=c(6,7))

  expect_equal(length(myResult),2)
  expect_is(myResult[[1]],"Scenario")

  #*************************************
  # View state class output
  # Add an (optional) raster attribute table. This is dataframe with ID, (optional) Color, and descriptor columns.
  # In this example, we load StateClass attributes from the library, then override the Colors.
  rat = datasheet(myResult[[1]],name="STSim_StateClass",optional=T)
  rat$Color = c("darkgreen","brown","wheat")

  myRasters = spatialData(myResult,sheet="STSim_OutputSpatialState",
                          iterations=seq(1),timesteps = seq(0,10,by=10),rat=rat)
  names(myRasters)
  expect_is(myRasters,"RasterStack")
  expect_equal(names(myRasters),c("STSim_OutputSpatialState.Scn6.It1.Ts0","STSim_OutputSpatialState.Scn6.It1.Ts10","STSim_OutputSpatialState.Scn7.It1.Ts0","STSim_OutputSpatialState.Scn7.It1.Ts10"))

  #check that returned attributes match entered attributes
  checkLevels = raster::levels(myRasters[[1]])[[1]]
  row.names(checkLevels) =checkLevels$ID
  expect_equal(checkLevels,merge(checkLevels,rat))
  #TO DO: why does levels fail in testing?

  #plot iteration 1 timestep 0
  view = myRasters[[1]]
  expect_equal(colortable(view),c("#A52A2AFF","#F5DEB3FF","#006400FF"))
  expect_equal(view@title,"STSim_OutputSpatialState.Scn6.It1.Ts0")
  expect_equal(freq(view)[,'count'],c(402,379,219,24))
  #not actually testing levelplot
  #levelplot(view,att="StateLabelXID",col.regions=colortable(view),main=view@title)

  #Change to automatically selected colors and save plot to pdf.
  newRat = raster::levels(view)[[1]]
  newRat$Color = brewer.pal(n = nrow(newRat), name = "Dark2")
  rasterAttributes(view) = newRat
  expect_equal(colortable(view),c("#1B9E77","#D95F02","#7570B3"))

  #*************************************
  # View spatial inputs
  mySpatialInputs = spatialData(myResult,sheet="STSim_InitialConditionsSpatial")
  age0=mySpatialInputs[["STSim_InitialConditionsSpatial.Scn6.It0000.Ts0000.age"]]
  expect_equal(cellStats(age0,"max"),100)

  #Build raster attribute table to view young only
  rat = data.frame(ID=raster::unique(age0))
  rat$isYoung[rat$ID<36]="young" #check young forest definition
  rat$isYoung[rat$ID>=36]="not young" #check young forest definition
  rat$Color = "wheat"; rat$Color[rat$isYoung=="young"]="darkgreen"
  rasterAttributes(age0) = rat
  checkLevels = raster::levels(age0)[[1]]
  row.names(checkLevels) = checkLevels$ID
  test = merge(checkLevels,rat)
  test=test[order(test$ID),]
  row.names(test)=test$ID
  expect_equal(checkLevels,test)

  ###############
  # Rearrange spatial outputs in a result scenario
  #set spatial options
  sheetName = "STime_Options"; mySheet = datasheet(myLibrary,name=sheetName)
  mySheet[1,"MultibandGroupingInternal"]="Multiband (iterations and timesteps combined)"
  silent = loadDatasheets(myProject,mySheet,name=sheetName)

  #Combining all spatial results into one multiband file will speed up loading.
  ret = multiband(myResult,action="apply")
  expect_equal(file.exists(paste0(filepath(myLibrary),".output/Scenario-6/Spatial/sc.tif")),TRUE)

})

#setwd('..')
setwd(retDir)
unlink("testLibs",recursive=T)
#getwd()
