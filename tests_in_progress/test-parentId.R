### ApexRMS
### 2024-10-08
### Below script tests the following functions:
### * parentId

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

# test that parentId can be used on scenario objects
test_that("can retrive parentId from Scenario", {
  expect_type(parentId(myScenario), "logical")
  expect_error(expect_error(parentId(myScenario)))
})

# test that errors are thrown when incorrect objects are used as arguments
test_that("errors work", {

  # create ojects to use as incorrect arguments
  vector <- c(1,2,3)
  list <- list(1,2,3)
  character <- "character"
  df <- data.frame(list(x = 1, y = 2, z  = 3))

  # test that errors are created
  expect_error(parentId("myScenario"))
  expect_error(parentId(vector))
  expect_error(parentId(list))
  expect_error(parentId(character))
  expect_error(parentId(df))
  expect_error(parentId(Scenario))
  expect_error(parentId(myProject))
  expect_error(parentId(myLibrary))
  expect_error(parentId(mySession))
})
