
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
  addPackage("helloworldSpatial")
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
