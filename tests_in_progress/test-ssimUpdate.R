### ApexRMS
### 2024-10-08
### Below script tests the following functions:
### * ssimUpdate

# load packages
library(rsyncrosim)
library(testthat)

# Setup ----
mySession <- session("C:/Program Files/SyncroSim Studio")
myLibraryName <- file.path(tempdir(),"testlib_update")
myLibrary <- ssimLibrary(name = myLibraryName,
                         session = mySession,
                         package = "stsim")
myProject <- project(myLibrary, project = "My Project")
myScenario <- scenario(myProject, scenario = "My Scenario")

# Tests ----
test_that("can use ssimUpdate", {
  expect_type(ssimUpdate(myScenario), "logical")
  expect_error(expect_error(ssimUpdate(myScenario)))

  expect_type(ssimUpdate(myProject), "logical")
  expect_error(expect_error(ssimUpdate(myProject)))

  expect_type(ssimUpdate(myLibrary), "logical")
  expect_error(expect_error(ssimUpdate(myLibrary)))
})

test_that("errors work", {
  # create ojects to use as incorrect arguments
  vector <- c(1,2,3)
  list <- list(1,2,3)
  character <- "character"
  df <- data.frame(list(x = 1, y = 2, z  = 3))

  # test that errors are created
  expect_error(ssimUpdate("myScenario"))
  expect_error(ssimUpdate(vector))
  expect_error(ssimUpdate(list))
  expect_error(ssimUpdate(character))
  expect_error(ssimUpdate(df))
  expect_error(ssimUpdate(Scenario))
  expect_error(ssimUpdate(mySession))
  expect_error(ssimUpdate(NULL))
})
