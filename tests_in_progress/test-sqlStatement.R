test_that("sqlStatement works", {

  installPackage("helloworldSpatial")
  myLibraryName <- file.path(tempdir(),"testlib_sqlStatement")
  mySession <- session()
  myLibrary <- ssimLibrary(name = myLibraryName,
                           session = mySession,
                           package = "helloworldSpatial",
                           template = "example-library")
  myProject <- project(myLibrary, project = "Definitions")
  myScenario <- scenario(myProject, scenario = "My Scenario")

  expect_error(mySQL <- sqlStatement(
    groupBy = c("ScenarioId", "Iteration", "Timestep"),
    aggregate = c("yCum"),
    aggregateFunction = "SUM",
    where = list(Timestep = c(0, 1, 2), Iteration = c(3, 4))), NA)

  resultScenario <- run(myScenario)
  myAggregatedDataFrame <- datasheet(resultScenario, name = "OutputDatasheet",
                                     sqlStatement = mySQL)
  # groupBy
  expect_error(sqlStatement(groupBy = "Iteration"), NA)
  expect_error(sqlStatement(groupBy = c("ScenarioId", "Iteration", "Timestep")), NA)

  # aggregate
  expect_error(sqlStatement(groupBy = "Iteration", aggregate = c("yCum")), NA)
  expect_error(sqlStatement(groupBy = c("ScenarioId", "Iteration", "Timestep"), aggregate = c("yCum")), NA)

  # aggregateFunction
  expect_error(sqlStatement(groupBy = "Iteration", aggregate = c("yCum"), aggregateFunction = "COUNT"), NA)
  expect_error(sqlStatement(groupBy = c("ScenarioId", "Iteration", "Timestep"), aggregate = c("yCum"), aggregateFunction = "SUM"), NA)

  # where
  expect_error(sqlStatement(groupBy = "Iteration", aggregate = c("yCum"), aggregateFunction = "COUNT", where = list(Timestep = c(0, 1, 2), Iteration = c(3, 4))), NA)
  expect_error(sqlStatement(groupBy = c("ScenarioId", "Iteration", "Timestep"), aggregate = c("yCum"), aggregateFunction = "SUM", where = list(Timestep = c(0, 1, 2))), NA)

  # functionality
  expect_error(mySQL <- sqlStatement(
    groupBy = c("ScenarioId", "Iteration", "Timestep"),
    aggregate = c("yCum"),
    aggregateFunction = "SUM",
    where = list(Timestep = c(0, 1, 2), Iteration = c(3, 4))), NA)

  expect_type(mySQL, "list")

  resultScenario <- run(myScenario)
  expect_error(datasheet(resultScenario, name = "OutputDatasheet",
                                     sqlStatement = mySQL),NA)

  helloscenario <- scenario(myLibrary, summary = TRUE)
  helloresult <- helloscenario[helloscenario$IsResult == "Yes",]
  helloID <- helloresult$ScenarioId
  delete(myLibrary, scenario = helloID, force = TRUE)

})

test_that("errors work", {
  installPackage("helloworldSpatial")
  myLibraryName <- file.path(tempdir(),"testlib_sqlStatement")
  mySession <- session()
  myLibrary <- ssimLibrary(name = myLibraryName,
                           session = mySession,
                           package = "helloworldSpatial",
                           template = "example-library")
  myProject <- project(myLibrary, project = "Definitions")
  myScenario <- scenario(myProject, scenario = "My Scenario")

  expect_error(sqlStatement(groupBy = myScenario))
  expect_error(sqlStatement(groupBy = myLibrary))
  expect_error(sqlStatement(groupBy = myProject))
  expect_error(sqlStatement(groupBy = library))
  expect_error(sqlStatement(groupBy = Iteration))
  expect_error(sqlStatement(groupBy = "Iteration", aggregate = yCum))
  expect_error(sqlStatement(groupBy = c("ScenarioId", "Iteration", "Timestep"), aggregate = c("yCum"), aggregateFunction = SUM))

})
