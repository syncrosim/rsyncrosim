### ApexRMS
### 2024-10-08
### Below script tests the following functions:
### * description

# load packages
library(rsyncrosim)
library(testthat)

# Setup ----
myLibraryName <- file.path(tempdir(), "testlib")
mySession <- session("C:/Program Files/SyncroSim Studio")
myLibrary <- ssimLibrary(name = myLibraryName, session = mySession)
myProject <- project(myLibrary, project = "My Project")
myScenario1 <- scenario(myProject, scenario = "My Scenario")
myScenario2 <- scenario(myProject, scenario = "My Scenario")

# Tests ----

# test for correct description output
test_that("can description all SsimObjects", {

  expect_equal(description(myProject), description(myLibrary))
  expect_equal(description(myScenario1), description(myProject))
  expect_type(description(myLibrary), "character")
  expect_type(description(myProject), "character")
  expect_type(description(myScenario1), "character")

  # test that descriptions are assigned correctly
  expect_equal(description(myLibrary) <- "my description", "my description")
  expect_equal(description(myProject) <- "my description", "my description")
  expect_equal(description(myScenario1) <- "my description", "my description")
  expect_equal(description(myScenario2) <- "123", "123")

  # test numeric descriptions
  expected1 <- description(myScenario2) <- 123
  expect_equal(expected1, 123)
  expect_type(expected1, "double")
})

# test that errors are thrown when incorrect objects are used as arguments
test_that("errors work", {
  # create ojects to use as incorrect arguments
  vector <- c(1,2,3)
  list <- list(1,2,3)
  character <- "character"
  df <- data.frame(list(x = 1, y = 2, z  = 3))

  # test errors
  expect_error(description("myLibrary"))
  expect_error(description(vector))
  expect_error(description(list))
  expect_error(description(character))
  expect_error(description(df))
  expect_error(description(library))
  expect_error((description(myLibrary)) <- mydescription)
})
