### ApexRMS
### 2024-10-08
### Below script tests the following functions:
### * parentId

# load packages
library(rsyncrosim)
library(testthat)

# Setup ----
mySession <- session("C:/Program Files/SyncroSim Studio")
myLibraryName <- file.path(tempdir(),"testlib.ssim")
myLibrary <- ssimLibrary(name = myLibraryName,
                         session = mySession,
                         package = "stsim")
myProject <- project(myLibrary, project = "Definitions")
myScenario <- scenario(myProject, scenario = "My Scenario")

# Tests ----

# test that scenarioId can be used on scenario objects
test_that("can retrive scenarioId from Scenario", {
  expect_type(scenarioId(myScenario), "double")
  expect_error(expect_error(scenarioId(myScenario)))

})

# test that errors are thrown when incorrect objects are used as arguments
test_that("errors work", {

  # create ojects to use as incorrect arguments
  vector <- c(1,2,3)
  list <- list(1,2,3)
  character <- "character"
  df <- data.frame(list(x = 1, y = 2, z  = 3))

  # test that errors are created
  expect_error(scenarioId("myScenario"))
  expect_error(scenarioId(vector))
  expect_error(scenarioId(list))
  expect_error(scenarioId(character))
  expect_error(scenarioId(df))
  expect_error(scenarioId(Scenario))
  expect_error(scenarioId(myLibrary))
  expect_error(scenarioId(mySession))
})
