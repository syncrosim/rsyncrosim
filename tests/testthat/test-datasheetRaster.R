
test_that("no errors", {
  addPackage("helloworldSpatial")
  mySession1 <- session()
  myLibrary1 <- ssimLibrary("C:/gitprojects/rsyncrosim/tests/temphelloworldspatial.ssim")
  myProject1 <- project(myLibrary1, project = "Definitions")
  myScenario1 <- scenario(myProject1, scenario = "My Scenario")
  resultScenario1 <- run(myScenario1)
  #
  addPackage("stsim")
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
  addPackage("stsim")
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
  addPackage("helloworldSpatial")
  mySession5 <- session()
  myLibrary5 <- ssimLibrary("C:/gitprojects/rsyncrosim/tests/temphelloworldspatial.ssim")
  myProject5 <- project(myLibrary5, project = "Definitions")
  myScenario5 <- scenario(myProject5, scenario = "My Scenario")
  resultScenario5 <- run(myScenario5)
  #
  addPackage("stsim")
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
