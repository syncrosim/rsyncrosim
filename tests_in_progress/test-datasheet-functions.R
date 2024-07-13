### ApexRMS
### 2024-07-11
### Below script tests the following functions:
### * datasheet
### * datasheetSpatRaster
### * saveDatasheet

# Setup ----
library(terra)
myLibraryPath <- "C:/gitprojects/rsyncrosim/tests_in_progress/test_library/spatial-example.ssim"
mySession <- session("C:/gitprojects/ssimbin3/")

# Tests ----
# Load existing library and runnable scenarios (14, 16)
myLib <- ssimLibrary(myLibraryPath, session = mySession)
myProj <- rsyncrosim::project(myLib, project=1)
myScn <- scenario(myProj, scenario = 14)
mySpatialScn <- scenario(myProj, scenario = 16)
mySpatialResultsScn <- scenario(myProj, scenario = 101)

# Test getting lists of datasheets for each ssimObject
datasheet(myLib)
datasheet(myProj)
datasheet(myScn)

# Load a library datasheet
mpDS <- datasheet(myLib, "core_Multiprocessing")

# Test modifying and saving
mpDS$EnableMultiScenario <- FALSE
saveDatasheet(myLib, mpDS, "core_Multiprocessing")
datasheet(myLib, "core_Multiprocessing")

# Load a project datasheet
terminology <- datasheet(myProj, "stsim_Terminology")
str(terminology)
levels(terminology$AmountUnits)
terminology$AmountUnits <- "acres"
saveDatasheet(myProj, terminology, "stsim_Terminology")

terminologyNoFactors <- datasheet(myProj, "stsim_Terminology", lookupsAsFactors = F)
str(terminologyNoFactors)

datasheet(myProj, "stsim_StateClass", summary = T, optional = T)

datasheet(myProj, name = "core_DistributionType")
datasheet(myProj, name = "core_DistributionType", returnInvisible = T)

# Load scenario datasheet
pipeline <- datasheet(myScn, name = "core_Pipeline")
str(pipeline)

datasheet(myScn, name = "stsim_Transition", includeKey = T)

datasheet(myScn, name = "stsim_Transition", optional =T)

datasheet(myScn, name = "stsim_Transition", returnScenarioInfo = T) # broken

# Load spatial data
ics <- datasheet(mySpatialScn, "stsim_InitialConditionsSpatial")
ics$AgeFileName

r <- datasheetSpatRaster(mySpatialScn, "stsim_InitialConditionsSpatial", 
                         column = "AgeFileName")
plot(r)

outputStates <- datasheet(mySpatialResultsScn, "stsim_OutputSpatialState")
outputStates

r <- datasheetSpatRaster(mySpatialResultsScn,
                         "stsim_OutputSpatialState",
                         column = "Filename")
plot(r)

r <- datasheetSpatRaster(mySpatialResultsScn,
                         "stsim_OutputSpatialState",
                         column = "Filename",
                         iteration = 2)
plot(r)

r <- datasheetSpatRaster(mySpatialResultsScn,
                        "stsim_OutputSpatialState",
                        column = "Filename",
                        iteration = 2,
                        timestep = 2004)
plot(r)

# Old code below for datasheet ----
mySession <- session()
installPackage("helloworldSpatial")
myLibraryName <- file.path(tempdir(),"testlib_datasheet")
myLibrary <- ssimLibrary(name = myLibraryName,
                         session = mySession,
                         package = "helloworldSpatial",
                         forceUpdate = TRUE,
                         overwrite = TRUE)
myProject <- project(myLibrary, project = "Definitions")
myScenario <- scenario(myProject, scenario = "My Scenario")
mySQL <- (sqlStatement(groupBy = c("ScenarioId"), aggregate = c("MinimumTimestep"), where = list(MinimumTimestep = c(1))))

test_that("no errors", {
  expect_error(datasheet(myScenario), NA)
  expect_error((datasheet(myScenario, name = NULL, summary = TRUE, optional = FALSE,
                          empty = FALSE, filterColumn = NULL, filterValue = NULL,
                          lookupsAsFactors = TRUE, includeKey = FALSE, forceElements = FALSE,
                          fastQuery = FALSE)), NA)
  expect_error((datasheet(myProject, summary = FALSE)), NA)
  expect_error((datasheet(myScenario, name = "RunControl", includeKey = TRUE, summary = "TRUE", optional = TRUE)), NA)
  #expect_error((datasheet(myScenario, name = "RunControl", forceElements = TRUE, fastQuery = TRUE)), NA)
  expect_error((datasheet(myScenario, name = "RunControl", empty = TRUE, lookupsAsFactors = FALSE)), NA)
  expect_error((datasheet(myScenario, name = "RunControl", sqlStatement = mySQL)), NA)
  expect_error((datasheet(myLibrary, summary = "CORE")), NA)
  expect_error((datasheet(myLibrary, name = "core_Backup")), NA)
  expect_error((datasheet(myScenario, summary = FALSE, empty = FALSE, lookupsAsFactors = FALSE, optional = FALSE)), NA)
})

test_that("correct statements ignored", {
  expect_equal(datasheet(myScenario), (datasheet(myScenario, name = NULL, summary = TRUE, optional = FALSE,
                                                 empty = FALSE, filterColumn = NULL, filterValue = NULL,
                                                 lookupsAsFactors = TRUE, includeKey = FALSE, forceElements = FALSE,
                                                 fastQuery = FALSE)))
  expect_equal((datasheet(myScenario, name = NULL, summary = TRUE, optional = FALSE,
                          empty = FALSE, filterColumn = NULL, filterValue = NULL,
                          lookupsAsFactors = TRUE, includeKey = FALSE, forceElements = FALSE,
                          fastQuery = FALSE)), datasheet(myScenario, name = NULL, summary = TRUE, optional = FALSE,
                               empty = TRUE, filterColumn = NULL, filterValue = NULL,
                               lookupsAsFactors = FALSE, includeKey = FALSE, forceElements = TRUE,
                               fastQuery = TRUE))
  #expect_equal((datasheet(myScenario, summary = FALSE, empty = FALSE, lookupsAsFactors = FALSE, optional = FALSE)), datasheet(myScenario, summary = FALSE, empty = FALSE, lookupsAsFactors = FALSE, optional = TRUE))
})

test_that("errors work", {
  expect_error(datasheet("myScenario"))
  expect_error(datasheet(myScenario, name = c(1,2,3)))
  expect_error(datasheet(myScenario, name = RunControl))
  expect_warning(expect_error(datasheet(myScenario, name = "STSim_RunControl")), "An STSim_ prefix for a datasheet name is no longer required.")
  expect_warning(datasheet(myScenario, name = "RunControl", project = "one"), "project argument is ignored when ssimObject is a Project/Scenario or list of these.")
  expect_warning(datasheet(myScenario, name = "RunControl", scenario = "one"))
  expect_error(datasheet(myScenario, name = "RunControl", summary = "YES"))
  expect_error(datasheet(myScenario, name = "RunControl", summary = TRUE, optional = "FALSE"))
  expect_error(datasheet(myScenario, name = "RunControl", summary = FALSE, optional = FALSE, empty = "FALSE"))
  expect_error(datasheet(myScenario, name = "RunControl", summary = FALSE,
                         optional = FALSE, empty = FALSE, lookupsAsFactors = "TRUE"))
  expect_error(datasheet(myScenario, name = "RunControl", summary = FALSE,
                         optional = FALSE, empty = FALSE, lookupsAsFactors = TRUE,
                         sqlStatement = "mySQL"))
  expect_error(datasheet(myScenario, name = "RunControl", summary = FALSE,
                         optional = FALSE, empty = FALSE, lookupsAsFactors = TRUE,
                         sqlStatement = mySQL, includeKey = "FALSE"))
  expect_error(datasheet(myScenario, name = "RunControl", summary = FALSE,
                         optional = FALSE, empty = FALSE, lookupsAsFactors = TRUE,
                         sqlStatement = mySQL, includeKey = FALSE, forceElements = "FALSE"))
  expect_error(datasheet(myScenario, name = "RunControl", summary = FALSE,
                         optional = FALSE, empty = FALSE, lookupsAsFactors = TRUE,
                         sqlStatement = mySQL, includeKey = FALSE, forceElements = FALSE,
                         fastQuery = "FALSE"))
  expect_error(datasheet(name = "RunControl"))
})

# Old code below for saveDatasheet ----

test_that("can retrive saveDatasheet", {
  myLibrary1 <- ssimLibrary(name = "C:/gitprojects/rsyncrosim/tests/tempstsim.ssim")
  myProject1 <- project(myLibrary1, project = "Definitions")
  myScenario1 <- scenario(myProject1, scenario = "My Scenario")
  
  #ssimObject and data and name arguments
  Project <- datasheet(myProject1, name = "TransitionType")
  Scenario1 <- datasheet(myScenario1, name = "InitialConditionsSpatial")
  Scenario2 <- datasheet(myScenario1, name = "RunControl")
  
  expect_message(saveDatasheet(myProject1, data = Project, name = "TransitionType"))
  expect_message((saveDatasheet(myScenario1, data = Scenario1, name = "InitialConditionsSpatial")))
  expect_message((saveDatasheet(myScenario1, data = Scenario2, name = "RunControl")))
  expect_warning(expect_message(expect_message(saveDatasheet(myScenario1, data = list(Scenario1, Scenario2), name = c("stsim_InitialConditionsSpatial", "stsim_RunControl"))))) #will get warning
  expect_warning(expect_message(expect_message(saveDatasheet(myScenario1, data = list(Project, Scenario1), name = c("stsim_TransitionType", "stsim_InitialConditionsSpatial"))))) #will get warning
  expect_message(expect_type(saveDatasheet(myProject1, data = Project, name = "TransitionType"), "logical"))
  expect_message(expect_equal(saveDatasheet(myProject1, data = Project, name = "TransitionType"), TRUE))
  
  #import argument
  installPackage("helloworldSpatial")
  myLibraryName <- file.path(tempdir(),"testlib_saveDatasheet")
  mySession <- session()
  myLibrary <- ssimLibrary(name = myLibraryName,
                           session = mySession,
                           package = "helloworldSpatial",
                           template = "example-library")
  myProject <- project(myLibrary, project = "Definitions")
  myScenario <- scenario(myProject, scenario = "My Scenario")
  myDatasheet <- datasheet(myScenario, name = "RunControl")
  myDatasheet$MaximumTimestep <- 10
  
  expect_message((saveDatasheet(ssimObject = myScenario, data = myDatasheet, name = "RunControl",
                                import = "TRUE")))
  expect_message((saveDatasheet(ssimObject = myScenario, data = myDatasheet, name = "RunControl",
                                import = TRUE)))
  expect_message((saveDatasheet(ssimObject = myScenario, data = myDatasheet, name = "RunControl",
                                import = FALSE)))
  
  #path argument
  expect_message((saveDatasheet(ssimObject = myScenario, data = myDatasheet, name = "RunControl",
                                path = tempdir())))
  expect_message((saveDatasheet(ssimObject = myScenario, data = myDatasheet, name = "RunControl",
                                path = "C:/gitprojects/rsyncrosim")))
  
  #fileData argument
  map1 <- datasheetRaster(myScenario, datasheet = "InputDatasheet",
                          column = "InterceptRasterFile")
  inRasters <- raster::stack(map1)
  inSheet <- datasheet(myScenario, name="InputDatasheet")
  inSheet[1,"InterceptRasterFile"] <- names(inRasters)[1]
  expect_message((saveDatasheet(myScenario, data=inSheet, name="InputDatasheet",
                                fileData=inRasters)))
  #append argument
  #expect_message(saveDatasheet(ssimObject = myScenario, data = myDatasheet, name = "RunControl", path = tempdir(), append = TRUE), "The transfer method is not valid for a single row data sheet.") #only throws error when do 'Run Tests'
  expect_message(saveDatasheet(ssimObject = myScenario, data = myDatasheet, name = "RunControl", path = tempdir(), append = FALSE), "Datasheet <helloworldSpatial_RunControl> saved")
  expect_message(saveDatasheet(myProject1, data = Project, name = "TransitionType", append = TRUE), "Datasheet <stsim_TransitionType> saved")
  expect_message(saveDatasheet(myProject1, data = Project, name = "TransitionType", append = FALSE), "Datasheet <stsim_TransitionType> saved")
  expect_message(saveDatasheet(myProject1, data = Project, name = "TransitionType", append = "FALSE"), "Datasheet <stsim_TransitionType> saved")
  
  #forceElements argument
  expect_message(expect_type(saveDatasheet(ssimObject = myScenario, data = myDatasheet, name = "RunControl", forceElements = FALSE), "logical"), "Datasheet <helloworldSpatial_RunControl> saved")
  expect_message(expect_type(saveDatasheet(ssimObject = myScenario, data = myDatasheet, name = "RunControl", forceElements = TRUE), "list"), "Datasheet <helloworldSpatial_RunControl> saved")
  expect_message(expect_type(saveDatasheet(myProject1, data = Project, name = "TransitionType", forceElements = TRUE), "list"), "Datasheet <stsim_TransitionType> saved")
  expect_message(expect_type(saveDatasheet(myProject1, data = Project, name = "TransitionType", forceElements = FALSE), "logical"), "Datasheet <stsim_TransitionType> saved")
  
  #force argument
  expect_message((saveDatasheet(ssimObject = myScenario, data = myDatasheet, name = "RunControl", force = FALSE)), "Datasheet <helloworldSpatial_RunControl> saved")
  expect_message((saveDatasheet(ssimObject = myScenario, data = myDatasheet, name = "RunControl", force = TRUE)), "Datasheet <helloworldSpatial_RunControl> saved")
  expect_message((saveDatasheet(myProject1, data = Project, name = "TransitionType", append = TRUE, force = TRUE)), "Datasheet <stsim_TransitionType> saved")
  expect_message((saveDatasheet(myProject1, data = Project, name = "TransitionType", append = FALSE, force = TRUE)), "Datasheet <stsim_TransitionType> saved")
  expect_message((saveDatasheet(myProject1, data = Project, name = "TransitionType", append = FALSE, force = FALSE)), "Datasheet <stsim_TransitionType> saved") ## expected prompt but not the case ##
  expect_message((saveDatasheet(myProject1, data = Project, name = "TransitionType", append = "FALSE", force = "TRUE")), "Datasheet <stsim_TransitionType> saved")
  
  #breakpoint argument
  expect_error((saveDatasheet(myScenario, data = myDatasheet, name = "RunControl", breakpoint = TRUE)), NA)
  expect_message((saveDatasheet(myScenario, data = myDatasheet, name = "RunControl", breakpoint = FALSE)), "Datasheet <helloworldSpatial_RunControl> saved")
  expect_message((saveDatasheet(myProject1, data = Project, name = "TransitionType", breakpoint = TRUE)), NA)
  expect_message((saveDatasheet(myProject1, data = Project, name = "TransitionType", breakpoint = FALSE)), "Datasheet <stsim_TransitionType> saved")
  expect_message((saveDatasheet(myProject1, data = Project, name = "TransitionType", breakpoint = "FALSE")), "Datasheet <stsim_TransitionType> saved")
  
})

test_that("errors work", {
  myLibrary1 <- ssimLibrary(name = "C:/gitprojects/rsyncrosim/tests/tempstsim.ssim")
  myProject1 <- project(myLibrary1, project = "Definitions")
  myScenario1 <- scenario(myProject1, scenario = "My Scenario")
  
  Project <- datasheet(myProject1, name = "TransitionType")
  Scenario1 <- datasheet(myScenario1, name = "InitialConditionsSpatial")
  Scenario2 <- datasheet(myScenario1, name = "RunControl")
  #######
  expect_error(saveDatasheet(myScenario1))
  expect_error(saveDatasheet(myScenario1, data = Scenario1))
  expect_error(saveDatasheet(myScenario1, name = "stsim_InitialConditionsSpatial"))
  expect_error(saveDatasheet(myProject1, data = Scenario1, name = "stsim_InitialConditionsSpatial"))
  expect_error(saveDatasheet(myLibrary1))
  expect_error(saveDatasheet(myScenario1, data = c(Scenario1, Scenario2), name = c("stsim_InitialConditionsSpatial", "stsim_RunControl")))
  expect_error(saveDatasheet(ssimObject = myScenario, data = myDatasheet, name = "RunControl",
                             path = errortest))
  expect_error(expect_message((saveDatasheet(ssimObject = myScenario, data = myDatasheet, name = "RunControl",
                                             import = NO))))
  expect_error((saveDatasheet(myScenario, data=inSheet, name="InputDatasheet",
                              fileData=list(inRasters))))
  expect_message(expect_error(saveDatasheet(myProject1, data = Project, name = "TransitionType", forceElements = "FALSE")), "Datasheet <stsim_TransitionType> saved")
  expect_error(expect_message(saveDatasheet(myProject1, data = Project, name = "TransitionType", append = NO), "Datasheet <stsim_TransitionType> saved"))
  expect_error(((saveDatasheet(myProject1, data = Project, name = "TransitionType", append = what, force = why))))
  expect_error((saveDatasheet(myProject1, data = Project, name = "TransitionType", breakpoint = NO)))
  expect_error((saveDatasheet(myProject1, data = Project, name = "TransitionType", breakpoint = "what")))
  
})

# Old code below for datasheetRaster ----

test_that("no errors", {
  installPackage("helloworldSpatial")
  mySession1 <- session()
  myLibrary1 <- ssimLibrary("C:/gitprojects/rsyncrosim/tests/temphelloworldspatial.ssim")
  myProject1 <- project(myLibrary1, project = "Definitions")
  myScenario1 <- scenario(myProject1, scenario = "My Scenario")
  resultScenario1 <- run(myScenario1)
  #
  installPackage("stsim")
  mySession2 <- session()
  myLibrary2 <- ssimLibrary("C:/gitprojects/rsyncrosim/tests/tempstsim.ssim")
  myScenario2 <- scenario(myLibrary2, scenario = 22)
  
  resultDatasheet1 <- datasheet(resultScenario1, name = "IntermediateDatasheet")
  outputRasterPaths1 <- resultDatasheet1$OutputRasterFile
  #
  expect_error((datasheetRaster(resultScenario1,datasheet = "IntermediateDatasheet", column = "OutputRasterFile", iteration = 3, timestep = 2)), NA)
  #expect_error((datasheetRaster(resultScenario1, datasheet = "IntermediateDatasheet",
  #  column = "OutputRasterFile",
  #  subset = expression(grepl("ts20", outputRasterPaths1, fixed = TRUE)))), NA)
  expect_error((datasheetRaster(resultScenario1,
                                datasheet = "IntermediateDatasheet",
                                column = "OutputRasterFile",
                                forceElements = TRUE)), NA)
  expect_error((datasheetRaster(myScenario2,
                                datasheet = "stsim_OutputSpatialState",
                                timestep = 5,
                                iteration = 5,
                                filterColumn = "Timestep", filterValue = "5")), NA)
  expect_error((datasheetRaster(myScenario2,
                                datasheet = "stsim_OutputSpatialState",
                                timestep = 5,
                                filterColumn = "Timestep", filterValue = "5",
                                pathOnly = TRUE)), NA)
  expect_error((datasheetRaster(myLibrary2,
                                scenario = 22,
                                datasheet = "stsim_OutputSpatialState",
                                timestep = 5,
                                iteration = 5,
                                filterColumn = "Timestep", filterValue = "5")), NA)
  expect_error(datasheetRaster(myScenario2,
                               datasheet = "stsim_OutputSpatialState",
                               timestep = c(5,6),
                               iteration = c(5,6)), NA)
})

test_that("correct equivalents", {
  installPackage("stsim")
  mySession4 <- session()
  myLibrary4 <- ssimLibrary("C:/gitprojects/rsyncrosim/tests/tempstsim.ssim")
  myScenario4 <- scenario(myLibrary4, scenario = 22)
  
  expect_equal((datasheetRaster(myScenario4,
                                datasheet = "stsim_OutputSpatialState",
                                timestep = 5,
                                iteration = 5,
                                filterColumn = "Timestep", filterValue = "5")), (datasheetRaster(myLibrary4,
                                                                                                 scenario = 22,
                                                                                                 datasheet = "stsim_OutputSpatialState",
                                                                                                 timestep = 5,
                                                                                                 iteration = 5,
                                                                                                 filterColumn = "Timestep", filterValue = "5")))
  expect_equal((datasheetRaster(myLibrary4,
                                scenario = 22,
                                datasheet = "stsim_OutputSpatialState",
                                timestep = 5,
                                iteration = 5,
                                filterColumn = "Timestep", filterValue = "5")), datasheetRaster(myLibrary4,
                                                                                                scenario = "Harvest 20 Hectares per Year - spatial ([16] @ 16-Jan-2023 2:50 PM)",
                                                                                                datasheet = "stsim_OutputSpatialState",
                                                                                                timestep = "5",
                                                                                                iteration = "5",
                                                                                                filterColumn = "Timestep", filterValue = "5"))
})

test_that("errors work", {
  installPackage("helloworldSpatial")
  mySession5 <- session()
  myLibrary5 <- ssimLibrary("C:/gitprojects/rsyncrosim/tests/temphelloworldspatial.ssim")
  myProject5 <- project(myLibrary5, project = "Definitions")
  myScenario5 <- scenario(myProject5, scenario = "My Scenario")
  resultScenario5 <- run(myScenario5)
  #
  installPackage("stsim")
  mySession6 <- session()
  myLibrary6 <- ssimLibrary("C:/gitprojects/rsyncrosim/tests/tempstsim.ssim")
  myScenario6 <- scenario(myLibrary6, scenario = 22)
  
  resultDatasheet5 <- datasheet(resultScenario5, name = "IntermediateDatasheet")
  outputRasterPaths5 <- resultDatasheet5$OutputRasterFile
  #
  
  expect_error(datasheetRaster(myLibrary5))
  expect_error(datasheetRaster(resultScenario5,
                               datasheet = IntermediateDatasheet,
                               column = "OutputRasterFile",
                               forceElements = TRUE))
  expect_error(datasheetRaster(resultScenario5,
                               datasheet = "IntermediateDatasheet",
                               column = OutputRasterFile,
                               forceElements = TRUE))
  expect_error(datasheetRaster(resultScenario5,
                               datasheet = "IntermediateDatasheet",
                               column = "OutputRFile",
                               forceElements = TRUE))
  expect_error(datasheetRaster(myLibrary6,
                               scenario = "22",
                               datasheet = "stsim_OutputSpatialState",
                               timestep = 5,
                               iteration = 5,
                               filterColumn = "Timestep", filterValue = "5"))
  expect_error(datasheetRaster(myLibrary6,
                               scenario = 22,
                               datasheet = "stsim_OutputSpatialState",
                               timestep = 5,
                               iteration = 5,
                               filterColumn = "TransitionTypeID"))
  expect_error(datasheetRaster(myLibrary6,
                               scenario = 22,
                               datasheet = "stsim_OutputSpatialState",
                               timestep = 5,
                               iteration = 5,
                               filterColumn = Timestep, filterValue = "5"))
  expect_error(datasheetRaster(resultScenario5,
                               datasheet = "IntermediateDatasheet",
                               column = "OutputRasterFile",
                               subset = expression("ts20")))
  #expect_error(expect_equal(resultRaster3, datasheetRaster(resultScenario1,
  # datasheet = "IntermediateDatasheet",
  # column = "OutputRasterFile")))
})
#test forceElements and pathOnly


