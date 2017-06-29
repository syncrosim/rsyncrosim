#library(testthat)
#setwd("C:/gitprojects/rsyncrosim")
#setwd(retDir)
retDir = getwd()
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
  myLibrary = ssimLibrary(name=libPath,forceUpdate=T,session=session(printCmd=T))

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
  expect_equal(names(myRasters),c("scn6.sc.ts0.it1","scn6.sc.ts5.it1","scn6.sc.ts10.it1",
                                  "scn7.sc.ts0.it1","scn7.sc.ts5.it1","scn7.sc.ts10.it1"))

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
  names(myTransitionGroup)
  expect_equal(names(myTransitionGroup),c("scn6.tg.17.ts1.it1","scn7.tg.17.ts1.it1"))
  
  mySpatialInputs = datasheetRaster(myResult,datasheet="STSim_InitialConditionsSpatial",column="AgeFileName")
  expect_equal(names(mySpatialInputs),c("scn6.It0000.Ts0000.age","scn7.It0000.Ts0000.age"))
  #RESUME HERE
  
  age0=mySpatialInputs[["scn7.It0000.Ts0000.age"]]
  
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
  
  
  
  mySpatialInputs = spatialData(myResult,sheet="STSim_InitialConditionsSpatial")
  age0=mySpatialInputs[["STSim_InitialConditionsSpatial.Scn6.It0000.Ts0000.age"]]
  expect_equal(cellStats(age0,"max"),100)

  #Build raster attribute table to view young only
  rat = data.frame(ID=raster::unique(age0))
  rat$isYoung[rat$ID<36]="young" #check young forest definition
  rat$isYoung[rat$ID>=36]="not young" #check young forest definition
  rat$Color = "wheat"; rat$Color[rat$isYoung=="young"]="darkgreen"
  ssimRatify(age0) = rat
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
  silent = saveDatasheet(myProject,mySheet,name=sheetName)

  #Combining all spatial results into one multiband file will speed up loading.
  ret = multiband(myResult,action="apply")
  expect_equal(file.exists(paste0(filepath(myLibrary),".output/Scenario-6/Spatial/sc.tif")),TRUE)

})

#setwd('..')
setwd(retDir)
unlink("testLibs",recursive=T)
#getwd()
