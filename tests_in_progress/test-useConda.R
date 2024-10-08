### ApexRMS
### 2024-10-08
### Below script tests the following functions:
### * parentId

# load packages
library(rsyncrosim)
library(testthat)

# Setup ----
mySession <- session("C:/Program Files/SyncroSim Studio")
libPath <- "tests_in_progress/test_library/spatial-example.ssim"
myLibrary <- ssimLibrary(name = libPath, session = mySession)
myProject <- project(myLibrary, project = 1)
myScenario <- scenario(myProject, scenario = 16)

# Tests ----
test_that("useConda works", {
  expect_error(useConda(myLibrary), NA)
  expect_type(useConda(myLibrary), "logical")
  expect_equal(useConda(myLibrary), FALSE)
  expect_error(useConda(myLibrary) <- TRUE, NA)
  expect_equal(useConda(myLibrary), TRUE)
  expect_error(useConda(myLibrary) <- "FALSE", NA)
  expect_equal(useConda(myLibrary), FALSE)
})

test_that("errors work", {
  expect_error(useConda("myLibrary"))
  expect_error(useConda(mySession))
  expect_error(useConda(myProject))
  expect_error(useConda(myScenario))

  expect_error(useConda(myLibrary) <- NO)
  expect_error(useConda(myLibrary) <- mySession)

})
