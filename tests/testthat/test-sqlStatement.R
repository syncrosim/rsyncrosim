test_that("multiplication works", {

  addPackage("helloworldSpatial")
  myLibraryName <- file.path(tempdir(),"testlib_sqlStatement")
  mySession <- session()
  myLibrary <- ssimLibrary(name = myLibraryName,
                           session = mySession,
                           package = "helloworldSpatial",
                           template = "example-library")
  myProject <- project(myLibrary, project = "Definitions")
  myScenario <- scenario(myProject, scenario = "My Scenario")

  expect_error(mySQL <- sqlStatement(
    groupBy = c("ScenarioID", "Iteration", "Timestep"),
    aggregate = c("yCum"),
    aggregateFunction = "SUM",
    where = list(Timestep = c(0, 1, 2), Iteration = c(3, 4))), NA)

  resultScenario <- run(myScenario)
  myAggregatedDataFrame <- datasheet(resultScenario, name = "OutputDatasheet",
                                     sqlStatement = mySQL)
})
