# # Copyright (c) 2019 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# # GPL v.3 License
# 
# #library(testthat)
# #setwd("C:/gitprojects/rsyncrosim")
# #setwd(retDir)
# retDir = getwd()
# unlink("testLibs",recursive=T)
# dir.create('testLibs')
# setwd("./testLibs")
# 
# test_that("Test simple spatial STSim example", {
#   skip_on_cran()
#   
#   library(raster)
# 
#   #get a local copy of the package demonstration library
#   unzip(system.file("extdata", "DemonstrationLibrary_ssim_backup.zip", package = "rsyncrosim"),
#         exdir=getwd(),overwrite=T)
#   
#   #*************************************
#   # View  "STSim_OutputSpatialState" results
#   myLibrary = ssimLibrary("Demonstration Library.ssim",session=session(sessionPath),forceUpdate=T)
# 
#   myProject = project(myLibrary,project=1)
#   
#   myResult=scenario(myProject,c(3,4)) #demonstrate handling of multiple result scenarios
#   expect_equal(length(myResult),2)
#   expect_is(myResult[[1]],"Scenario")
#   
#   #*************************************
#   # View state class output
#   # Add an (optional) raster attribute table. This is dataframe with ID, (optional) Color, and descriptor columns.
#   # In this example, we load StateClass attributes from the library, then override the Colors.
#   # Off for v0.1. rat and ssimRatify are disabled in first release of rsyncrosim, so this section of code will not work.
#   
#   if(1){
#     myRasters = datasheetRaster(myResult,datasheet="STSim_OutputSpatialState",
#                               iteration=seq(1),timestep = seq(0,2,by=2))
#     expect_is(myRasters,"RasterStack")
#     expect_equal(names(myRasters),c("scn3.sc.it1.ts0","scn3.sc.it1.ts2",
#                                     "scn4.sc.it1.ts0","scn4.sc.it1.ts2"))
#     
#   }else{
#     rat = datasheet(myResult[[1]],name="STSim_StateClass",optional=T)
#     rat$Color = c("darkgreen","brown","wheat")
#     
#     myRasters = datasheetRaster(myResult,datasheet="STSim_OutputSpatialState",
#                                 iteration=seq(1),timestep = seq(0,2,by=2),rat=rat)
#     expect_is(myRasters,"RasterStack")
#     expect_equal(names(myRasters),c("scn3.sc.it1.ts0","scn3.sc.it1.ts2",
#                                     "scn4.sc.it1.ts0","scn4.sc.it1.ts2"))
#     
#     checkLevels = raster::levels(myRasters[[1]])[[1]]
#     row.names(checkLevels) =checkLevels$ID
#     expect_equal(checkLevels$hexColor,c("#A52A2AFF","#F5DEB3FF","#006400FF"))
#     checkLevels$hexColor=NULL
#     expect_equal(checkLevels,merge(checkLevels,rat))
#     
#     view = myRasters[[1]]  
#     expect_equal(raster::freq(view)[,'count'],c(402,379,219,24))
#     
#     #ssimLevelplot(myRasters[[1]],attribute="StateLabelXID",main=names(myRasters)[1]) 
#     #Not sure how to test graphical output.
#   }
#   
#   #*************************************
#   # View spatial inputs
#   # Off for v0.1. rat and ssimRatify are disabled in first release of rsyncrosim, so this section of code will not work.
# 
#   myTransitionGroup = datasheetRaster(myLibrary,scenario=as.numeric(names(myResult)),datasheet="STSim_OutputSpatialTransition",
#                                       timestep=2,iteration=1,subset=expression(TransitionGroupID=="Succession"))
#   #NOTE: setting scenario is slower than using a list of scenario objects.
#   #NOTE: using subset slows things down because must get lookups for datasheet. 
#   expect_equal(names(myTransitionGroup),c("scn3.tg_26.it1.ts2","scn4.tg_26.it1.ts2"))
#   
#   mySpatialInputs = datasheetRaster(myResult,datasheet="STSim_InitialConditionsSpatial",column="AgeFileName")
#   expect_equal(names(mySpatialInputs),c("scn3.initial.age","scn4.initial.age"))
#    
#   age0=mySpatialInputs[["scn4.initial.age"]]
#   expect_equal(raster::cellStats(age0,"max")<=100,T)
# 
#   ##################
#   #Set spatial inputs in a new library.
#   #Off in v0.1
#   if(0){
#   newScenario = scenario(myProject,scenario="NewScn")
#   
#   ageMap = datasheetRaster(myResult[[1]],datasheet="STSim_InitialConditionsSpatial",column="AgeFileName")
#   stateMap = datasheetRaster(myResult[[1]],datasheet="STSim_InitialConditionsSpatial",column="StateClassFileName")
#   stratumMap = datasheetRaster(myResult[[1]],datasheet="STSim_InitialConditionsSpatial",column="StratumFileName")
#   inRasters=raster::stack(ageMap,stateMap,stratumMap)
#   names(inRasters)=gsub(".it0.ts0","",names(inRasters),fixed=T)
# 
#   sheetName = "STSim_InitialConditionsSpatial"
#   inSheet = datasheet(newScenario,name=sheetName)
#   inSheet[1,"StratumFileName"]=names(inRasters)[3]
#   inSheet[1,"StateClassFileName"]=names(inRasters)[2]
#   inSheet[1,"AgeFileName"]=names(inRasters)[1]
#   ret=saveDatasheet(newScenario,data=inSheet,name=sheetName,fileData=inRasters)
#   expect_equal(file.exists(paste0(filepath(newScenario),".input/Scenario-",scenarioId(newScenario),"/STSim_InitialConditionsSpatial/initial.age.tif")),T)
#   ds = datasheet(newScenario,name="STSim_InitialConditionsSpatialProperties")
#   expect_equal(ds$NumRows,dim(ageMap)[1])
#   
#   #Change the extent of the input maps
#   inExtent = raster::extent(inRasters)
#   outExtent = inExtent
#   outExtent@xmax = 5;outExtent@ymax=5
#   newRasters=raster::crop(inRasters,outExtent)
#   anotherScenario = scenario(myProject,"Another New Scenario")
#   ret=saveDatasheet(anotherScenario,data=inSheet,name=sheetName,fileData=newRasters)
#   ds = datasheet(anotherScenario,name="STSim_InitialConditionsSpatialProperties")
#   expect_equal(ds$NumRows,dim(newRasters)[1])
#   }
#   ###############
#   # Rearrange spatial outputs in a result scenario
#   #set spatial options
#   #Off in v0.1
#   #sheetName = "STime_Options"; mySheet = datasheet(myLibrary,name=sheetName)
#   #mySheet[1,"MultibandGroupingInternal"]="Multiband (iterations and timesteps combined)"
#   #silent = saveDatasheet(myProject,mySheet,name=sheetName)
#   #Combining all spatial results into one multiband file will speed up loading.
#   #ret = multiband(myResult[[1]],action="apply")
#   #expect_equal(file.exists(paste0(filepath(myLibrary),".output/Scenario-6/STSim_OutputSpatialState/sc.tif")),TRUE)
# 
# })
# 
# #setwd('..')
# setwd(retDir)
# unlink("testLibs",recursive=T)
# #getwd()
