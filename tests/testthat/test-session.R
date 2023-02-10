test_that("can retrive session", {
  mySession <- session()
  myLibraryName <- file.path(tempdir(),"testlib_datasheet")
  myLibrary <- ssimLibrary(name = myLibraryName,
                           session = mySession,
                           package = "helloworldSpatial",
                           forceUpdate = TRUE,
                           overwrite = TRUE)
  myProject <- project(myLibrary, project = "Definitions")
  myScenario <- scenario(myProject, scenario = "My Scenario")

  # x argument
  expect_error(session(x = filepath(session(myProject))), NA)
  expect_error(session(x = myLibrary), NA)
  expect_error(session(x = myProject), NA)
  expect_error(session(x = myScenario), NA)
  expect_error(session("C:/Program Files/SyncroSim"), NA)
  expect_error(session(x = NULL), NA)

  # silent argument
  expect_error(session(x = filepath(session(myProject)), silent = TRUE), NA)
  expect_error(session(x = myLibrary, silent = TRUE), NA)
  expect_error(session(x = myProject, silent = TRUE), NA)
  expect_error(session(x = myScenario, silent = TRUE), NA)
  expect_error(session("C:/Program Files/SyncroSim", silent = TRUE), NA)
  expect_error(session(x = NULL, silent = TRUE), NA)

  expect_error(session(x = filepath(session(myProject)), silent = FALSE), NA)
  expect_error(session(x = myLibrary, silent = FALSE), NA)
  expect_error(session(x = myProject, silent = FALSE), NA)
  expect_error(session(x = myScenario, silent = FALSE), NA)
  expect_error(session("C:/Program Files/SyncroSim", silent = FALSE), NA)
  expect_error(session(x = NULL, silent = FALSE), NA)

  # prinCmd argument
  expect_output(expect_error(session(x = filepath(session(myProject)), silent = TRUE, printCmd = TRUE), NA))
  expect_equal(session(x = myLibrary, silent = TRUE, printCmd = TRUE), session(x = myLibrary))
  expect_equal(session(x = myProject, silent = TRUE, printCmd = TRUE), session(x = myProject))
  expect_equal(session(x = myScenario, silent = TRUE, printCmd = TRUE), session(x = myScenario))
  expect_output(expect_error(session("C:/Program Files/SyncroSim", silent = FALSE, printCmd = TRUE), NA))
  expect_output(expect_error(session(x = NULL, silent = TRUE, printCmd = TRUE), NA))

  expect_error(expect_output(session(x = filepath(session(myProject)), silent = TRUE, printCmd = FALSE)))
  expect_equal(session(x = myLibrary, silent = FALSE, printCmd = FALSE), session(x = myLibrary))
  expect_equal(session(x = myProject, silent = FALSE, printCmd = FALSE), session(x = myProject))
  expect_equal(session(x = myScenario, silent = FALSE, printCmd = FALSE), session(x = myScenario))
  expect_error(expect_output(session("C:/Program Files/SyncroSim", silent = FALSE, printCmd = FALSE)))
  expect_error(expect_output(session(x = NULL, silent = TRUE, printCmd = FALSE)))

  # condaFilepath argument
  expect_error(session(x = filepath(session(myProject)), condaFilepath = NULL), NA)
  expect_error(session(x = myLibrary, condaFilepath = NULL), NA)
  expect_error(session(x = myProject, condaFilepath = "C:/Users/GabrielleEdnie/SyncroSim/Conda"), NA)
  expect_error(session(x = myScenario, condaFilepath = "C:/Users/GabrielleEdnie/SyncroSim/Conda"), NA)
  expect_error(session("C:/Program Files/SyncroSim", condaFilepath = "C:/Users/GabrielleEdnie/SyncroSim/Conda"), NA)
  expect_error(session(x = NULL, condaFilepath = NULL), NA)

  #ssimObject and value argument
  expect_error(session(ssimObject = myLibrary) <- session("C:/Program Files/SyncroSim", condaFilepath = "C:/Users/GabrielleEdnie/SyncroSim/Conda"), NA)
  expect_error(session(ssimObject = myProject) <- session(x = myLibrary, condaFilepath = NULL), NA)
  expect_error(session(ssimObject = myScenario) <- session(x = NULL, condaFilepath = NULL), NA)
  expect_error(session(ssimObject = myProject) <- session(x = filepath(session(myProject)), condaFilepath = NULL), NA)
  expect_error(session(ssimObject = myLibrary) <- session(x = myScenario, condaFilepath = "C:/Users/GabrielleEdnie/SyncroSim/Conda"), NA)
})

test_that("errors work", {
  mySession <- session()
  myLibraryName <- file.path(tempdir(),"testlib_datasheet")
  myLibrary <- ssimLibrary(name = myLibraryName,
                           session = mySession,
                           package = "helloworldSpatial",
                           forceUpdate = TRUE,
                           overwrite = TRUE)
  myProject <- project(myLibrary, project = "Definitions")
  myScenario <- scenario(myProject, scenario = "My Scenario")


  expect_error(session(x = myScenarioX))
  expect_warning((session(x = "C:/Program/SyncroSim")))
  expect_error(session(ssimObject = myLibrary))
  expect_error(session(myProject) <- ProjectX)
  expect_error(session("C:/gitprojects/rsyncrosim/tests/tempstsim.ssim") <- session(x = filepath(session(myProject)), condaFilepath = "default"))
  expect_error(session(x = 1))

})
