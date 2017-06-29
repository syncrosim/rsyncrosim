#library(testthat)
#setwd("C:/gitprojects/rsyncrosim")
#setwd(retDir)
retDir = getwd()
if(dir.exists("./testLibs")){
  libRoot = "./testLibs"
  libName = "ST-Sim Spatial Tutorial"
  libPath = paste0(libRoot,"/",libName,"/",libName,".ssim")
  ret=delete(libPath,force=T)
}
unlink("testLibs",recursive=T)
dir.create('testLibs')
setwd("./testLibs")

test_that("Test simple spatial STSim example", {
  libRoot = getwd()
  libName = "ST-Sim Spatial Tutorial"
  libPath = paste0(libRoot,"/",libName,"/",libName,".ssim")
  #TO DO: fix this when ftp is working again
  if(!file.exists(libPath)){
    zipPath = paste0(libRoot,"/",libName,".zip")
    if(!file.exists(zipPath)){
      libURL = "http://www.apexrms.com//wp-content//uploads//ST-Sim-Spatial-Tutorial.zip"
      download.file(libURL, zipPath)
    }else{
      file.exists("C:/Temp/ST-Sim Spatial Tutorial.zip")
      file.copy("C:/Temp/ST-Sim Spatial Tutorial.zip",libRoot)
    }
    unzip(zipPath,exdir=paste0(libRoot,"/",libName),overwrite=T,unzip = "unzip")
  }

  #*************************************
  # View  "STSim_OutputSpatialState" results
  myLibrary = ssimLibrary(name=libPath,forceUpdate=T,session=session(printCmd=F))

  project(myLibrary)
  myProject = project(myLibrary,project=1)
  
  ret=run(myProject,5,summary=T,jobs=3)
  
  resultScns = scenario(myProject,results=T)
  myResult = scenario(myProject,scenario=tail(resultScns,n=2)$scenarioId)
  expect_equal(length(myResult),2)
  expect_is(myResult[[1]],"Scenario")

  #*************************************
  # View state class output
  rat = datasheet(myResult[[1]],name="STSim_StateClass",optional=T)
  rat$Color = c("darkgreen","brown","wheat")
  
  myRasters = datasheetRaster(myResult,datasheet="STSim_OutputSpatialState",
                              iteration=seq(1),timestep = seq(0,10,by=5),rat=rat)
  expect_is(myRasters,"RasterStack")
  expect_equal(names(myRasters),c("scn6.sc.it1.ts0","scn6.sc.it1.ts5","scn6.sc.it1.ts10",
                                  "scn7.sc.it1.ts0","scn7.sc.it1.ts5","scn7.sc.it1.ts10"))

  checkLevels = raster::levels(myRasters[[1]])[[1]]
  row.names(checkLevels) =checkLevels$ID
  expect_equal(checkLevels$hexColor,c("#A52A2AFF","#F5DEB3FF","#006400FF"))
  checkLevels$hexColor=NULL
  expect_equal(checkLevels,merge(checkLevels,rat))

  view = myRasters[[1]]  
  expect_equal(raster::freq(view)[,'count'],c(402,379,219,24))
  
  #ssimLevelplot(myRasters[[1]],attribute="StateLabelXID",main=names(myRasters)[1]) 
  #Not sure how to test graphical output.
  
  #*************************************
  # View spatial inputs
  myTransitionGroup = datasheetRaster(myLibrary,scenario=as.numeric(names(myResult)),datasheet="STSim_OutputSpatialTransition",timestep=1,iteration=1,subset=expression(TransitionGroupID=="Fire"))
  #NOTE: setting scenario is slower than using a list of scenario objects.
  #NOTE: using subset slows things down because must get lookups for datasheet. 
  names(myTransitionGroup)
  expect_equal(names(myTransitionGroup),c("scn6.tg.17.it1.ts1","scn7.tg.17.it1.ts1"))
  
  mySpatialInputs = datasheetRaster(myResult,datasheet="STSim_InitialConditionsSpatial",column="AgeFileName")
  expect_equal(names(mySpatialInputs),c("scn6.age.it0.ts0","scn7.age.it0.ts0"))
  
  age0=mySpatialInputs[["scn7.age.it0.ts0"]]
  
  expect_equal(raster::cellStats(age0,"max"),98)

  ##################
  #Set spatial inputs in a new library.
  newScenario = scenario(myProject,scenario="NewScn")
  
  ageMap = datasheetRaster(myResult[[1]],datasheet="STSim_InitialConditionsSpatial",column="AgeFileName")
  stateMap = datasheetRaster(myResult[[1]],datasheet="STSim_InitialConditionsSpatial",column="StateClassFileName")
  stratumMap = datasheetRaster(myResult[[1]],datasheet="STSim_InitialConditionsSpatial",column="StratumFileName")
  inRasters=raster::stack(ageMap,stateMap,stratumMap)
  names(inRasters)=gsub(".it0.ts0","",names(inRasters),fixed=T)

  sheetName = "STSim_InitialConditionsSpatial"
  inSheet = datasheet(newScenario,name=sheetName)
  inSheet[1,"StratumFileName"]=names(inRasters)[3]
  inSheet[1,"StateClassFileName"]=names(inRasters)[2]
  inSheet[1,"AgeFileName"]=names(inRasters)[1]
  ret=saveDatasheet(newScenario,data=inSheet,name=sheetName,fileData=inRasters)
  
  expect_equal(file.exists(paste0(filepath(newScenario),".input/Scenario-8/STSim_InitialConditionsSpatial/age.tif")),T)
  ds = datasheet(newScenario,name="STSim_InitialConditionsSpatialProperties")
  expect_equal(ds$NumRows,dim(ageMap)[1])
  
  #Change the extent of the input maps
  inExtent = raster::extent(inRasters)
  outExtent = inExtent
  outExtent@xmax = 500;outExtent@ymax=500
  newRasters=raster::crop(inRasters,outExtent)
  anotherScenario = scenario(myProject,"Another New Scenario")
  saveDatasheet(anotherScenario,data=inSheet,name=sheetName,fileData=newRasters)
  ds = datasheet(anotherScenario,name="STSim_InitialConditionsSpatialProperties")
  expect_equal(ds$NumRows,dim(newRasters)[1])
  
  ###############
  # Rearrange spatial outputs in a result scenario
  #set spatial options
  sheetName = "STime_Options"; mySheet = datasheet(myLibrary,name=sheetName)
  mySheet[1,"MultibandGroupingInternal"]="Multiband (iterations and timesteps combined)"
  silent = saveDatasheet(myProject,mySheet,name=sheetName)

  #Combining all spatial results into one multiband file will speed up loading.
  ret = multiband(myResult[[1]],action="apply")
  expect_equal(file.exists(paste0(filepath(myLibrary),".output/Scenario-6/STSim_OutputSpatialState/sc.tif")),TRUE)

})

#setwd('..')
setwd(retDir)
unlink("testLibs",recursive=T)
#getwd()
