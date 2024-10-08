### ApexRMS
### 2024-10-08
### Below script tests the following functions:
### * condaFilepath

# load packages
library(rsyncrosim)
library(testthat)

# Setup ----
mySession <- session("C:/Program Files/SyncroSim Studio")
myCondaFilepath <- "C:/Users/HannahAdams/miniconda3"

# set up library
mySession1 <- session("C:/Program Files/SyncroSim Studio")
mySession2 <- session("C:/Program Files/SyncroSim Studio")
myLibraryName <- file.path(tempdir(),"testlib.ssim")
myLibrary1 <- ssimLibrary(name = myLibraryName, session = mySession1)
myLibrary2 <- ssimLibrary(name = myLibraryName, session = mySession2)
myProject1 <- project(myLibrary1, project = "My Project")
myProject2 <- project(myLibrary2, project = "My Project")
myScenario1 <- scenario(myProject1, scenario = "My Scenario")
myScenario2 <- scenario(myProject2, scenario = "My Scenario")

# Tests ----
test_that("name class", {
  expect_equal(condaFilepath(mySession1), NULL)
  expect_type(condaFilepath(mySession1), "NULL")
  expect_equal((condaFilepath(mySession2) <- myCondaFilepath), myCondaFilepath)
  expect_type((condaFilepath(mySession2) <- myCondaFilepath), "character")
})

# test that errors are thrown when incorrect arguments are given
test_that("errors work", {
  #expect_warning(expect_warning(condaFilepath("mySession1")))
  expect_error(condaFilepath(mySession2) <- C(myCondaFilepathv))
  expect_error(condaFilepath(list(myCondaFilepath)))
  #expect_warning(expect_warning(condaFilepath("myLibrary1")))
  #expect_warning(expect_warning(condaFilepath("C:/Users/GabrielleEdnie/miniconda3")))
  expect_error(condaFilepath(myLibrary1))
  expect_error(condaFilepath(myProject1))
  expect_error(condaFilepath(myScenario1))
})
