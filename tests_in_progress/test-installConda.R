### ApexRMS
### 2024-10-08
### Below script tests the following functions:
### * filepath

# load packages
library(rsyncrosim)
library(testthat)

# Setup ----
#myLibraryName <- file.path(tempdir(),"testlib")
#mySession <- session()
#myLibrary <- ssimLibrary(name = myLibraryName, session = mySession)
mySession <- session("C:/Program Files/SyncroSim Studio")
myLibrary <- ssimLibrary(name = "/tests/tempstsim.ssim", session = mySession)
myProject <- project(myLibrary, project = "My Project")
myScenario <- scenario(myProject, scenario = "My Scenario")

# Tests ----

# test that conda can be installed in the session
test_that("can installConda", {
  expect_error(expect_error(installConda(mySession))) # only works if leave field empty, can't select a session object
  expect_error(expect_error(installConda()))
})

# test that errors are thrown when incorrect objects are used as arguments
test_that("errors work", {
  # create ojects to use as incorrect arguments
  vector <- c(1,2,3)
  list <- list(1,2,3)
  character <- "character"
  df <- data.frame(list(x = 1, y = 2, z  = 3))

  # test that errors are created
  expect_error(installConda("myLibrary"))
  expect_error(installConda(vector))
  expect_error(installConda(list))
  expect_error(installConda(character))
  expect_error(installConda(df))
  expect_error(installConda(library))
  expect_error(installConda(myProject))
  expect_error(installConda(myScenario))
  expect_error(installConda(myLibrary))
})
