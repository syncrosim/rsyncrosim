### ApexRMS
### 2024-10-08
### Below script tests the following functions:
### * projectId

# load packages
library(rsyncrosim)
library(testthat)

# Setup ----
mySession <- session("C:/Program Files/SyncroSim Studio")
myLibraryName <- file.path(tempdir(), "testlib.ssim")
myLibrary <- ssimLibrary(name = myLibraryName, session = mySession)
myProject <- project(myLibrary, project = "My Project")
myScenario <- scenario(myProject, scenario = "My Scenario")

# Tests ----

# test that projectId can be used on scenario objects
test_that("can retrive projectId from Scenario", {
  expect_type(projectId(myScenario), "double")
  expect_error(expect_error(projectId(myScenario)))
  expect_type(projectId(myProject), "double")
  expect_error(expect_error(projectId(myProject)))
})

# test that errors are thrown when incorrect objects are used as arguments
test_that("errors work", {
  # create ojects to use as incorrect arguments
  vector <- c(1,2,3)
  list <- list(1,2,3)
  character <- "character"
  df <- data.frame(list(x = 1, y = 2, z  = 3))

  # test that errors are created
  expect_error(projectId("myScenario"))
  expect_error(projectId(vector))
  expect_error(projectId(list))
  expect_error(projectId(character))
  expect_error(projectId(df))
  expect_error(projectId(Scenario))
  expect_error(projectId(myLibrary))
  expect_error(projectId(mySession))
})
