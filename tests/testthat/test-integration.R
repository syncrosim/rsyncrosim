# Copyright (c) 2019 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# GPL v.3 License

retDir <- getwd()
unlink("testLibs", recursive = T)
dir.create("testLibs")
setwd("./testLibs")

myses <- session()

test_that("Test simple non-spatial STSim example - assumes that SyncroSim is installed.", {
  skip_on_cran()

  # Create the project definition
  libPath <- paste0(getwd(), "/ST-Sim-Command-Line.ssim")
  ret <- delete(libPath, force = T)
  myLibrary <- ssimLibrary(session = myses, name = libPath)
  myProject <- project(myLibrary, project = "ST-Sim Demonstration")

  #***********************************
  # Cover types and state classes
  sheetName <- "Stratum"
  mySheet <- datasheet(myProject, name = sheetName, empty = F, optional = T)
  mySheet[1, "Name"] <- "Entire Forest"
  mySheet[1, "Description"] <- "Another description"
  ret <- saveDatasheet(myProject, mySheet, name = sheetName)
  expect_equal(names(datasheet(myProject, name = sheetName, empty = T, optional = F)), "Name") # returns only truly optional columns
  expect_equal(datasheet(myProject, name = sheetName, empty = F, optional = F)$Description, "Another description") # returns optional columns and columns with data
  expect_equal(names(datasheet(myProject, name = sheetName, empty = F, optional = T)), c("Name", "ID", "Color", "Legend", "Description")) # returns all columns

  sheetName <- "StateClass"
  expect_warning(
    datasheet(myProject, name = sheetName, empty = F),
    "StateLabelXID depends on stsim_StateLabelX. You should load stsim_StateLabelX before setting stsim_StateClass.",
    "StateLabelYID depends on stsim_StateLabelY. You should load stsim_StateLabelY before setting stsim_StateClass."
  )

  sheetName <- "StateLabelY"
  mySheet <- data.frame(Name = "All")
  ret <- saveDatasheet(myProject, mySheet, name = sheetName)

  sheetName <- "StateLabelX"
  mySheet <- data.frame(Name = c("Coniferous", "Deciduous", "Mixed"))
  ret <- saveDatasheet(myProject, mySheet, name = sheetName)

  sheetName <- "StateClass"
  mySheet <- datasheet(myProject, name = sheetName, empty = T)
  expect_equal(levels(mySheet$StateLabelXID), c("Coniferous", "Deciduous", "Mixed"))
  mySheet[1:3, "StateLabelXID"] <- levels(mySheet$StateLabelXID) # Valid values
  mySheet$StateLabelYID <- levels(mySheet$StateLabelYID)[1] # Valid values
  mySheet$Name <- paste0(mySheet$StateLabelXID, ":", mySheet$StateLabelYID)
  ret <- saveDatasheet(myProject, mySheet, name = sheetName)
  # expect_equal(is.element("StateClassID",names(datasheet(myProject,sheetName,includeKey=T))),T) #include primary key for datasheet

  #***********************************
  # Transitions
  sheetName <- "TransitionGroup"
  mySheet <- data.frame(Name = c("Fire", "Harvest", "Succession"))
  ret <- saveDatasheet(myProject, mySheet, name = sheetName)
  ret <- saveDatasheet(myProject, mySheet, name = "TransitionType")

  sheetName <- "TransitionTypeGroup"
  mySheet <- data.frame(TransitionTypeID = c("Fire", "Harvest", "Succession"))
  mySheet$TransitionGroupID <- mySheet$TransitionTypeID
  ret <- saveDatasheet(myProject, mySheet, name = sheetName)

  #****************
  # Age type
  sheetName <- "AgeType"
  mySheet <- data.frame(Frequency = 1, MaximumAge = 100)
  ret <- saveDatasheet(myProject, mySheet, name = sheetName)

  #*************************************
  # Build Scenario That Contains Shared Parameters
  #*************************************
  myScenario <- scenario(myProject, scenario = "Dependency Scenario")

  #**************
  # Run control
  sheetName <- "RunControl"
  mySheet <- data.frame(MinimumIteration = 1, MaximumIteration = 2, MinimumTimestep = 0, MaximumTimestep = 10)
  ret <- saveDatasheet(myScenario, mySheet, name = sheetName)

  #**************************
  # Deterministic transitions
  sheetName <- "DeterministicTransition"
  mySheet <- datasheet(myScenario, name = sheetName, optional = T, empty = T)
  mySheet <- addRow(mySheet, data.frame(StateClassIDSource = "Coniferous:All", StateClassIDDest = "Coniferous:All", AgeMin = 21, Location = "C1"))
  expect_equal(mySheet$Location, "C1")
  mySheet <- addRow(mySheet, data.frame(StateClassIDSource = "Deciduous:All", StateClassIDDest = "Deciduous:All", Location = "A1"))
  mySheet <- addRow(mySheet, data.frame(StateClassIDSource = "Mixed:All", StateClassIDDest = "Mixed:All", AgeMin = 11, Location = "B1"))
  expect_equal(mySheet$AgeMin, c(21, NA, 11))
  expect_equal(levels(mySheet$StateClassIDSource), c("Coniferous:All", "Deciduous:All", "Mixed:All"))
  ret <- saveDatasheet(myScenario, mySheet, name = sheetName)

  #*************************
  # Probabilistic transitions
  sheetName <- "Transition"
  mySheet <- data.frame(
    StateClassIDSource = c("Coniferous:All", "Coniferous:All", "Deciduous:All", "Deciduous:All", "Mixed:All", "Mixed:All"),
    StateClassIDDest = c("Deciduous:All", "Deciduous:All", "Deciduous:All", "Mixed:All", "Coniferous:All", "Deciduous:All"),
    TransitionTypeID = c("Fire", "Harvest", "Fire", "Succession", "Succession", "Fire"),
    Probability = c(0.01, 1, 0.002, 0.1, 0.1, 0.005),
    AgeMin = c(NA, 40, NA, 10, 20, NA)
  )
  mySheet$StratumIDSource <- "Entire Forest"
  mySheet$StratumIDDest <- "Entire Forest"
  ret <- saveDatasheet(myScenario, mySheet, name = sheetName)

  #********************
  # Initial conditions
  sheetName <- "InitialConditionsNonSpatial"
  mySheet <- data.frame(TotalAmount = 100, NumCells = 100)
  ret <- saveDatasheet(myScenario, mySheet, name = sheetName)

  sheetName <- "InitialConditionsNonSpatialDistribution"
  mySheet <- data.frame(
    StateClassID = c("Coniferous:All", "Deciduous:All", "Mixed:All"),
    AgeMin = c(20, NA, 11),
    AgeMax = c(100, 10, 20),
    RelativeAmount = c(20, 40, 40)
  )
  mySheet$StratumID <- "Entire Forest"
  ret <- saveDatasheet(myScenario, mySheet, name = sheetName)

  #*************************************
  # Add No Harvest Scenario
  #*************************************
  myScenario <- scenario(myProject, scenario = "No Harvest")

  ret <- dependency(myScenario, dependency = "Dependency Scenario") # set dependency
  expect_equal(dependency(myScenario)$name, "Dependency Scenario") # now there is a dependency
  ret <- dependency(myScenario, dependency = "Dependency Scenario", remove = T, force = T)
  expect_equal(nrow(dependency(myScenario)), 0) # dependency has been removed
  ret <- dependency(myScenario, dependency = "Dependency Scenario") # set dependency

  #******************
  # Transition targets
  sheetName <- "TransitionTarget"
  mySheet <- data.frame(TransitionGroupID = "Harvest", Amount = 0)
  ret <- saveDatasheet(myScenario, mySheet, name = sheetName)

  #*************************************
  # Add Harvest Scenario
  #*************************************
  myScenario <- scenario(myProject, scenario = "Harvest", sourceScenario = "No Harvest")

  #******************
  # Transition targets
  sheetName <- "TransitionTarget"
  mySheet <- data.frame(TransitionGroupID = "Harvest", Amount = 20)
  ret <- saveDatasheet(myScenario, mySheet, name = sheetName)

  #********************************
  # Run scenarios
  #******************************
  myResults <- run(myProject, scenario = c("Harvest", "No Harvest"), jobs = 4)
  expect_is(myResults[[1]], "Scenario")

  otherResults <- run(myScenario, jobs = 4, summary = T)
  expect_is(otherResults, "data.frame")

  expect_output(runLog(myResults[[1]]), "STARTING SIMULATION") # displays and returns a multiline string
  expect_equal(parentId(myResults[[1]]), 3)

  #********************************
  # See results
  #******************************
  sheetName <- "OutputStratumState"
  mySQL <- sqlStatement(groupBy = c("ScenarioID", "Iteration", "Timestep", "StateLabelXID"), aggregate = c("Amount"))
  outStatesAllAges <- datasheet(myResults, name = sheetName, sqlStatement = mySQL)
  expect_equal(setdiff(unique(outStatesAllAges$Timestep), seq(from = 0, to = 10)), numeric(0))
  expect_equal(setdiff(unique(outStatesAllAges$Iteration), seq(from = 1, to = 2)), numeric(0))
  expect_equal(setdiff(unique(outStatesAllAges$ParentName), c("Harvest", "No Harvest")), character(0))
  expect_equal(setdiff(unique(outStatesAllAges$StateLabelXID), c("Coniferous", "Deciduous", "Mixed")), character(0))
})

setwd(retDir)
unlink("testLibs", recursive = T)
