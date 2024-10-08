### ApexRMS
### 2024-10-08
### Below script tests the following functions:
### * filepath

# load packages
library(rsyncrosim)
library(testthat)

# Setup ----
myLibraryName <- file.path(tempdir(), "testlib")
mySession <- session("C:/Program Files/SyncroSim Studio")
myLibrary <- ssimLibrary(name = myLibraryName, session = mySession)
myProject <- project(myLibrary, project = "My Project")
myScenario <- scenario(myProject, scenario = "My Scenario")

# Tests ----

# test that filepath can be used on all syncrosim objects
test_that("can filepath all SsimObjects", {
  expect_type(filepath(myLibrary), "character")
  expect_type(filepath(myProject), "character")
  expect_type(filepath(myScenario), "character")
  expect_type(filepath(mySession), "character")
})

# test that errors are thrown when incorrect objects are used as arguments
test_that("errors work", {
  # create ojects to use as incorrect arguments
  vector <- c(1,2,3)
  list <- list(1,2,3)
  character <- "character"
  df <- data.frame(list(x = 1, y = 2, z  = 3))

  # test that errors are created
  expect_error(filepath("myLibrary"))
  expect_error(filepath(vector))
  expect_error(filepath(list))
  expect_error(filepath(character))
  expect_error(filepath(df))
  expect_error(filepath(library))
})
