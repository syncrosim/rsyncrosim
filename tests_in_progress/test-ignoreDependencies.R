### ApexRMS
### 2024-10-08
### Below script tests the following functions:
### * filepath

# load packages
library(rsyncrosim)
library(testthat)

# Setup ----
myLibraryName <- file.path(tempdir(), "test")
mySession <- session("C:/Program Files/SyncroSim Studio")
myLibrary <- ssimLibrary(name = myLibraryName, session = mySession, package = "stsim")
myProject <- project(myLibrary, project = "Definitions")
myScenario <- scenario(myProject, scenario = "My Scenario")

# Tests ----

# test ignoreDependencies can be used on scenario objects
test_that("can ignoreDependencies all SsimObjects", {
  expect_error(expect_error(ignoreDependencies(myScenario)))
  expect_type(ignoreDependencies(myScenario), "NULL")
  expect_error(expect_error(ignoreDependencies(myScenario) <- "stsim_RunControl,stsim_TransitionTarget"))

})

# test that errors are thrown when incorrect objects are used as arguments
test_that("errors work", {
  # create ojects to use as incorrect arguments
  vector <- c(1,2,3)
  list <- list(1,2,3)
  character <- "character"
  df <- data.frame(list(x = 1, y = 2, z  = 3))

  # test that errors are created
  expect_error(ignoreDependencies("myLibrary"))
  expect_error(ignoreDependencies(vector))
  expect_error(ignoreDependencies(list))
  expect_error(ignoreDependencies(character))
  expect_error(ignoreDependencies(df))
  expect_error(ignoreDependencies(library))
  expect_error(ignoreDependencies(myLibrary))
  expect_error(ignoreDependencies(myProject))
  expect_error(ignoreDependencies(mySession))
  expect_error(ignoreDependencies(myScenario, "stsim_RunControl"))
  expect_error(ignoreDependencies("stsim_RunControl"))
  expect_error(expect_error(ignoreDependencies(myScenario) <- stsim_RunControl,stsim_TransitionTarget))
  expect_error(ignoreDependencies(myScenario) <- stsim_RunControl)
})
