### ApexRMS
### 2024-10-08
### Below script tests the following functions:
### * dateModified

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

# retrieve date modified for the library, project, and scenario
expected1 <- dateModified(myLibrary)
expected2 <- dateModified(myProject)
expected3 <- dateModified(myScenario)

# make objects for error testing
vector <- c(1, 2, 3)
list <- list(1, 2, 3)
character <- "character"
df <- data.frame(list(x = 1, y = 2, z  = 3))

# test the library, project, and scenario dates
test_that("can dateModified all SsimObjects", {
  expect_equal(expected2, NULL)
  expect_equal(expected3, NULL)
  expect_type(expected1, "character")
  expect_type(expected2, "NULL")
  expect_type(expected3, "NULL")
})

# test that errors are thrown when incorrect objects are used as arguments
test_that("errors work", {
  expect_error(dateModified("myLibrary"))
  expect_error(dateModified(vector))
  expect_error(dateModified(list))
  expect_error(dateModified(character))
  expect_error(dateModified(df))
  expect_error(dateModified(library))
})
