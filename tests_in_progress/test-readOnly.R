### ApexRMS
### 2024-10-08
### Below script tests the following functions:
### * readOnly

# load packages
library(rsyncrosim)
library(testthat)

# Setup ----
mySession <- session("C:/Program Files/SyncroSim Studio")

# Tests ----

# test that the function works on all syncrosim objects
test_that("can readOnly all SsimObjects", {

  # set up library
  myLibraryname <- file.path(tempdir(), "testlib")
  myLibrary <- ssimLibrary(name = myLibraryname, session = mySession)
  myProject <- project(myLibrary, project = "Definitions")
  myScenario <- scenario(myProject, scenario = "My Scenario")

  # run tests on ssim scenario
  expect_equal(readOnly(myScenario), FALSE)
  expect_type(readOnly(myScenario), "logical")
  expect_error(expect_error(readOnly(myScenario) <- TRUE))
  expect_equal(readOnly(myScenario), TRUE)
  expect_type(readOnly(myScenario), "logical")

  # run tests on ssim library
  expect_equal(readOnly(myLibrary), FALSE)
  expect_type(readOnly(myLibrary), "logical")
  expect_error(expect_error(readOnly(myLibrary) <- TRUE))
  expect_equal(readOnly(myLibrary), TRUE)
  expect_type(readOnly(myLibrary), "logical")

  # run tests on ssim project
  expect_equal(readOnly(myProject), FALSE)
  expect_type(readOnly(myProject), "logical")
  expect_error(expect_error(readOnly(myProject) <- TRUE))
  expect_equal(readOnly(myProject), TRUE)
  expect_type(readOnly(myProject), "logical")

})

# test that function creates errors when incorrect objects are used as arguments
test_that("errors work", {

  # set up library
  myLibraryname <- file.path(tempdir(), "testlib")
  myLibrary <- ssimLibrary(name = myLibraryname, session = mySession)
  myProject <- project(myLibrary, project = "Definitions")
  myScenario <- scenario(myProject, scenario = "My Scenario")

  # create ojects to use as incorrect arguments
  vector <- c(1,2,3)
  list <- list(1,2,3)
  character <- "character"
  df <- data.frame(list(x = 1, y = 2, z  = 3))

  # test that errors are created
  expect_error(readOnly(mySession))
  expect_error(readOnly(vector))
  expect_error(readOnly(list))
  expect_error(readOnly(character))
  expect_error(readOnly(df))
  expect_error(readOnly(library))
  expect_error(readOnly(myScenario, TRUE))
  expect_error(readOnly(myScenario) <- "TRUE")
})
