### ApexRMS
### 2024-10-08
### Below script tests the following functions:
### * info

# load packages
library(rsyncrosim)
library(testthat)

# Setup ----
myLibraryName <- file.path(tempdir(),"testlib")
mySession <- session("C:/Program Files/SyncroSim Studio")
myLibrary <- ssimLibrary(name = myLibraryName, session = mySession)
myProject <- project(myLibrary, project = "My Project")
myScenario <- scenario(myProject, scenario = "My Scenario")

# Tests ----

# test that info can be used on library objects
test_that("can retrive info from Library", {
  expect_type(info(myLibrary), "list")
  expect_s3_class(info(myLibrary), "data.frame")
  expect_error(expect_error(info(myLibrary)))
})

# test that errors are thrown when incorrect objects are used as arguments
test_that("errors work", {
  # create ojects to use as incorrect arguments
  vector <- c(1,2,3)
  list <- list(1,2,3)
  character <- "character"
  df <- data.frame(list(x = 1, y = 2, z  = 3))

  # test that errors are created
  expect_error(info("myLibrary"))
  expect_error(info(vector))
  expect_error(info(list))
  expect_error(info(character))
  expect_error(info(df))
  expect_error(info(library))
  expect_error(info(myProject))
  expect_error(info(myScenario))
  expect_error(info(mySession))
})
