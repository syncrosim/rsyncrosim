### ApexRMS
### 2024-07-11
### Below script tests the following functions:
### * session
### * version
### * filepath
### * condaFilepath
### * silent
### * printCmd

# Setup ----
myLibraryName1 <- file.path(tempdir(),"testlib")
myLibraryName2 <- file.path(tempdir(),"mylib")
mySession <- session("C:/gitprojects/ssimbin3/")

# Tests ----
# If no path provided, should find default location of session
mySession2 <- session()
mySession2

# If path to older version of SyncroSim provided, should throw an error
mySession2 <- session("C:/Program Files/SyncroSim/")

# Should be rsyncrosim session object
mySession

# Version should be SyncroSim 3.x.x
version(mySession)

# Filepath should be same as specified above
filepath(mySession)

# Should return NULL or path
condaFilepath(mySession)

# Change silent status of session (TRUE is default)
silent(mySession)
silent(mySession) <- FALSE
silent(mySession)

mySession2 <- session("c:/gitprojects/ssimbin3/", silent = F)
silent(mySession2)

# Change printCmd status of session (FALSE is default)
printCmd(mySession)
mySession2 <- session("c:/gitprojects/ssimbin3/", printCmd = TRUE)
printCmd((mySession2))

# Should be able to grab session from library, project, scenario, or folder
myLibrary1 <- ssimLibrary(myLibraryName1, session = mySession)
session(myLibrary1)

myProject <- project(myLibrary1, "Definitions")
session(myProject)

myScenario <- scenario(myLibrary1, "My Scenario")
session(myScenario)

myFolder <- folder(myProject, "My Folder")
session(myFolder)

# Old code below for session ----
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

# Old code below for version ----
test_that("version works", {
  mySession <- session()
  
  expect_error(version(mySession), NA)
  expect_type(version(mySession), "character")
  expect_error(version(), NA)
  expect_type(version(), "character")
  
})

test_that("errors work", {
  myLibraryName <- file.path(tempdir(),"testlib_sqlStatement")
  mySession <- session()
  myLibrary <- ssimLibrary(name = myLibraryName,
                           session = mySession,
                           package = "helloworldSpatial",
                           template = "example-library")
  myProject <- project(myLibrary, project = "Definitions")
  myScenario <- scenario(myProject, scenario = "My Scenario")
  
  expect_error(version("mySession"))
  expect_error(version(myLibrary))
  expect_error(version(myProject))
  expect_error(version(myScenario))
  expect_error(version(1))
  
})

# Old tests below for printCmd ----
test_that("can retrive printCmd from Session", {
  mySession <- session()
  myLibrary <- ssimLibrary(name = "C:/gitprojects/rsyncrosim/tests/tempstsim.ssim")
  myProject <- project(myLibrary, project = "My Project")
  myScenario <- scenario(myProject, scenario = "My Scenario")
  
  expect_type(printCmd(mySession), "logical")
  expect_error(expect_error(printCmd(mySession)))
  expect_error(expect_error(printCmd()))
  expect_equal(printCmd(mySession), printCmd())
  
})

test_that("errors work", {
  mySession <- session()
  myLibrary <- ssimLibrary(name = "C:/gitprojects/rsyncrosim/tests/tempstsim.ssim")
  myProject <- project(myLibrary, project = "My Project")
  myScenario <- scenario(myProject, scenario = "My Scenario")
  
  character <- "character"
  
  #expect_warning(expect_warning((printCmd("mySession")))) expect warning function doesn't work for Ctrl + Shift + T
  #expect_warning(expect_warning(printCmd("character")))
  expect_error(printCmd(c(1,2,3)))
  expect_error(printCmd(list(1,2,3)))
  #expect_warning(expect_warning(printCmd(character)))
  expect_error(printCmd(data.frame(list(x = 1, y = 2, z  = 3))))
  expect_error(printCmd(Session))
  expect_error(printCmd(myProject))
  expect_error(printCmd(myLibrary))
  expect_error(printCmd(myScenario))
})

# Old tests below for silent ----

test_that("can silent all SsimObjects", {
  myLibraryname <- file.path(tempdir(), "testlib")
  mySession <- session()
  myLibrary <- ssimLibrary(name = myLibraryname, session = mySession)
  myProject <- project(myLibrary, project = "Definitions")
  myScenario <- scenario(myProject, scenario = "My Scenario")
  
  expect_equal(silent(mySession), TRUE)
  expect_type(silent(mySession), "logical")
  expect_error(expect_error(silent(mySession) <- FALSE))
  expect_equal(silent(mySession), FALSE)
  expect_type(silent(mySession), "logical")
  
  expect_equal(silent(session("C:/Program Files/SyncroSim")), TRUE)
  expect_type(silent(session("C:/Program Files/SyncroSim")), "logical")
  #expect_error(expect_error(silent(session("C:/Program Files/SyncroSim")) <- FALSE))
  #expect_equal(silent(session("C:/Program Files/SyncroSim")), FALSE)
  #expect_type(silent(session("C:/Program Files/SyncroSim")), "logical")
  
})

test_that("errors work", {
  myLibraryname <- file.path(tempdir(), "testlib")
  mySession <- session()
  myLibrary <- ssimLibrary(name = myLibraryname, session = mySession)
  myProject <- project(myLibrary, project = "Definitions")
  myScenario <- scenario(myProject, scenario = "My Scenario")
  
  vector <- c(1,2,3)
  list <- list(1,2,3)
  character <- "character"
  df <- data.frame(list(x = 1, y = 2, z  = 3))
  
  expect_error(silent(myScenario))
  expect_error(silent(myLibrary))
  expect_error(silent(myProject))
  expect_error(silent(vector))
  expect_error(silent(list))
  expect_error(silent(df))
  expect_error(silent(library))
  expect_error(silent(myScenario, TRUE))
  expect_error(silent(mySession) <- "TRUE")
})



