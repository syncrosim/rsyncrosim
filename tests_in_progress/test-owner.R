### ApexRMS
### 2024-10-07
### Below script tests the following functions:
### * owner

# load packages
library(rsyncrosim)
library(testthat)

# Setup ----
mySession <- session("C:/Program Files/SyncroSim Studio")

# Tests ----

# test that an owner can be created for all SsimObjects
test_that("can owner all SsimObjects", {

  # set up library
  myLibraryname <- file.path(tempdir(), "testlib")
  myLibrary <- ssimLibrary(name = myLibraryname, session = mySession)
  myProject <- project(myLibrary, project = "Definitions")
  myScenario <- scenario(myProject, scenario = "My Scenario")

  # run tests on ssim scenario
  # expect_equal(owner(myScenario), NULL) # error; is "N/A"
  # expect_type(owner(myScenario), "NULL") # error; is character
  expect_equal(owner(myScenario), "N/A")
  expect_type(owner(myScenario), "character")
  expect_error(expect_error(owner(myScenario) <- "Scenario"))
  expect_equal(owner(myScenario), "Scenario")
  expect_type(owner(myScenario), "character")

  # run tests on ssim library
  expect_equal(owner(myLibrary), "N/A")
  expect_type(owner(myLibrary), "character")
  expect_error(expect_error(owner(myLibrary) <- "Library"))
  expect_equal(owner(myLibrary), "Library")
  expect_type(owner(myLibrary), "character")

  # run tests on ssim project
  # expect_equal(owner(myProject), NULL) # error; is "N/A"
  # expect_type(owner(myProject), "NULL") # error; is character
  expect_equal(owner(myProject), "N/A")
  expect_type(owner(myProject), "character")
  expect_error(expect_error(owner(myProject) <- "Project"))
  expect_equal(owner(myProject), "Project")
  expect_type(owner(myProject), "character")
})

# test that errors are created when incorrect objects are used as arguments
test_that("errors work", {

  # set up library
  myLibraryname <- file.path(tempdir(), "testlib")
  myLibrary <- ssimLibrary(name = myLibraryname, session = mySession)
  myProject <- project(myLibrary, project = "Definitions")
  myScenario <- scenario(myProject, scenario = "My Scenario")

  # create ojects to use as incorrect arguments
  vector <- c(1, 2, 3)
  list <- list(1, 2, 3)
  character <- "character"
  df <- data.frame(list(x = 1, y = 2, z  = 3))

  # test that errors are created
  expect_error(owner("myLibrary"))
  expect_error(owner(vector))
  expect_error(owner(list))
  expect_error(owner(character))
  expect_error(owner(df))
  expect_error(owner(library))
  expect_error(owner(myScenario, "scenario"))
  expect_error(owner(myScenario) <- stsim_RunControl)
})
