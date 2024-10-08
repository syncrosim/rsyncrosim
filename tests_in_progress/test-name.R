### ApexRMS
### 2024-10-04
### Below script tests the following functions:
### * name

# load packages
library(rsyncrosim)
library(testthat)

# Setup ----
mySession <- session("C:/Program Files/SyncroSim Studio")
myLibraryName <- file.path(tempdir(), "lib")
myLibrary <- ssimLibrary(name = myLibraryName, session = mySession, package = "stsim")
myProject <- project(myLibrary, project = "Definitions")
myScenario <- scenario(myProject, scenario = "My Scenario")

# Tests ----

# test that all syncrosim objects can be named
test_that("can name all SsimObjects", {

  # run tests on ssim scenario
  expect_equal(name(myScenario), "My Scenario")
  expect_type(name(myScenario), "character")
  expect_error(expect_error(name(myScenario) <- "scenario"))
  expect_equal(name(myScenario), "scenario")

  # run tests on ssim library
  expect_equal(name(myLibrary), "lib")
  expect_type(name(myLibrary), "character")
  expect_error(expect_error(name(myLibrary) <- "Library"))
  expect_equal(name(myLibrary), "Library")

  # run tests on ssim project
  expect_equal(name(myProject), "Definitions")
  expect_type(name(myProject), "character")
  expect_error(expect_error(name(myProject) <- "Project"))
  expect_equal(name(myProject), "Project")
})

# test that errors are thrown when incorrect objects are used as arguments
test_that("errors work", {

  # create ojects to use as incorrect arguments
  vector <- c(1, 2, 3)
  list <- list(1, 2, 3)
  character <- "character"
  df <- data.frame(list(x = 1, y = 2, z  = 3))

  # test that errors are created
  expect_error(name("myLibrary"))
  expect_error(name(vector))
  expect_error(name(list))
  expect_error(name(character))
  expect_error(name(df))
  expect_error(name(library))
  expect_error(name(myScenario, "scenario"))
  expect_error(name(myScenario) <- stsim_RunControl)
})
