### ApexRMS
### 2024-10-08
### Below script tests the following functions:
### * sqlStatement

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

# test using a library with stsim
test_that("sqlStatement works", {

  expect_error(mySQL <- sqlStatement(groupBy = c("Iteration"), aggregate = c("Timestep"), aggregateFunction = "SUM", where = list(Timestep = c(0, 1, 2), Iteration = c(3, 4))), NA)

  # run scenario and test
  resultScenario <- run(myScenario)
  myAggregatedDataFrame <- datasheet(resultScenario, name = "stsim_OutputSpatialAge", sqlStatement = mySQL)

  # groupBy
  expect_error(sqlStatement(groupBy = "Iteration"), NA)

  # aggregate
  expect_error(sqlStatement(groupBy = "Iteration", aggregate = c("Timestep")), NA)

  # aggregateFunction
  expect_error(sqlStatement(groupBy = "Iteration", aggregate = c("Timestep"), aggregateFunction = "COUNT"), NA)
  expect_error(sqlStatement(groupBy = "Iteration", aggregate = c("Timestep"), aggregateFunction = "SUM"), NA)

  # where
  expect_error(sqlStatement(groupBy = "Iteration", aggregate = c("Timestep"), aggregateFunction = "COUNT", where = list(Timestep = c(0, 1, 2), Iteration = c(3, 4))), NA)
  expect_error(sqlStatement(groupBy = "Iteration", aggregate = c("Timestep"), aggregateFunction = "SUM", where = list(Timestep = c(0, 1, 2))), NA)

  # functionality
  expect_error(mySQL <- sqlStatement(groupBy = "Iteration", aggregate = c("Timestep"), aggregateFunction = "SUM", where = list(Timestep = c(0, 1, 2), Iteration = c(3, 4))), NA)
  expect_type(mySQL, "list")

  # run scenario and test
  resultScenario <- run(myScenario)
  expect_error(datasheet(resultScenario, name = "stsim_OutputSpatialAge", sqlStatement = mySQL),NA)

  # delete results
  helloscenario <- scenario(myLibrary, summary = TRUE)
  helloresult <- helloscenario[helloscenario$IsResult == "Yes",]
  helloID <- helloresult$ScenarioId
  delete(myLibrary, scenario = helloID, force = TRUE)
})

# test that errors are thrown when incorrect objects are used as arguments
test_that("errors work", {
  expect_error(sqlStatement(groupBy = myScenario))
  expect_error(sqlStatement(groupBy = myLibrary))
  expect_error(sqlStatement(groupBy = myProject))
  expect_error(sqlStatement(groupBy = library))
  expect_error(sqlStatement(groupBy = Iteration))
  expect_error(sqlStatement(groupBy = "Iteration", aggregate = yCum))
  expect_error(sqlStatement(groupBy = c("ScenarioId", "Iteration", "Timestep"), aggregate = c("yCum"), aggregateFunction = SUM))
})
