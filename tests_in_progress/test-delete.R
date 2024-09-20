### ApexRMS
### 2024-09-20
### Below script tests the following functions:
### * delete

## https://debruine.github.io/post/interactive-test/

# load packages
library(rsyncrosim)
library(testthat)

# test that all SsimObjects can be deleted
test_that("can delete all SsimObjects", {

  # Setup ----
  myLibraryName <- file.path(tempdir(), "testlib")
  mySession <- session("C:/Program Files/SyncroSim Studio")
  myLibrary <- ssimLibrary(name = myLibraryName, session = mySession)
  myProject <- project(myLibrary, project = "My Project")
  myScenario <- scenario(myProject, scenario = "My Scenario")

  # add stsim package
  addPackage(myLibrary, "stsim", version = "4.0.0")

  # Tests ----
  # Delete using ssimObject argument
  expect_type(delete(myScenario, force = TRUE), "logical")
  expect_type(delete(myProject, force = TRUE), "logical") # error
  expect_type(delete(myLibrary, force = TRUE), "logical") # error

  # create library, project, and scenario
  myLibrary <- ssimLibrary(name = myLibraryName, session = mySession)
  myProject <- project(myLibrary, project = "My Project")
  myScenario <- scenario(myProject, scenario = "My Scenario")

  # Delete using project argument
  project(myLibrary, project = "To Delete")
  expect_type(delete(myLibrary, project = "To Delete", force = TRUE), "logical") # error
  new <- project(myLibrary, project = "New")
  ID <- projectId(new)
  expect_type(delete(myLibrary, project = c(1, ID), force = TRUE), "list") # error

  # create library, project, and scenarios
  myLibrary <- ssimLibrary(name = myLibraryName, session = mySession)
  myProject <- project(myLibrary, project = "My Project")
  myScenario1 <- scenario(myProject, scenario = "My Scenario 1")
  myScenario2 <- scenario(myProject, scenario = "My Scenario 2")
  sID1 <- scenarioId(myScenario1)
  sID2 <- scenarioId(myScenario2)
  myProject2 <- project(myLibrary, project = "My Project")
  myScenario3 <- scenario(myProject2, scenario = "My Scenario 3")

  # Delete using scenario argument
  expect_type(delete(myProject, scenario = c(sID1, sID2), force = TRUE), "list")
  myScenario1 <- scenario(myProject, scenario = "My Scenario 1")
  expect_equal(delete(myLibrary, scenario = "My Scenario 1", force = TRUE), TRUE)
  expect_type(delete(myLibrary, scenario = "My Scenario 3", force = TRUE), "logical")

  # create library, project, and scenario
  myLibrary <- ssimLibrary(name = myLibraryName, session = mySession)
  myProject <- project(myLibrary, project = "My Project")
  myScenario <- scenario(myProject, scenario = "My Scenario")

  # Delete using datasheet argument
  # need to add stsim package first
  expect_equal(delete(myScenario, datasheet = "StateClass", force = TRUE), TRUE) # error
  expect_type(delete(myProject, datasheet = "StateClass", force = TRUE), "logical") # error

})

# test that trying to delete non-SsimObjects will raise errors
test_that("errors work", {

  # setup ----
  myLibraryName <- file.path(tempdir(), "testlib")
  mySession <- session("C:/Program Files/SyncroSim Studio")
  myLibrary <- ssimLibrary(name = myLibraryName, session = mySession)
  myProject <- project(myLibrary, project = "My Project")
  myScenario <- scenario(myProject, scenario = "My Scenario")

  # create non-ssim objects to test with
  vector <- c(1, 2, 3)
  list <- list(1, 2, 3)
  character <- "character"
  df <- data.frame(list(x = 1, y = 2, z  = 3))

  # Tests ----
  expect_error(delete(myLibraryName, force = TRUE))
  expect_error(delete("myLibrary"))
  expect_error(delete(vector))
  expect_error(delete(list))
  expect_error(delete(character))
  expect_error(delete(df))
  expect_error(delete(library))

  expect_error(delete(myProject, datasheet = c("Stratum", "AgeType"), force = TRUE))
  expect_error(delete(myProject, scenario = Mycenario, force = TRUE))
  expect_error(delete(myScenario, datasheet = StateClass, force = TRUE))
})
