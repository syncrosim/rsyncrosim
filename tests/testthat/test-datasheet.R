mySession <- session()
addPackage("helloworldSpatial")
myLibraryName <- file.path(tempdir(),"testlib_datasheet")
myLibrary <- ssimLibrary(name = myLibraryName,
                         session = mySession,
                         package = "helloworldSpatial",
                         forceUpdate = TRUE,
                         overwrite = TRUE)
myProject <- project(myLibrary, project = "Definitions")
myScenario <- scenario(myProject, scenario = "My Scenario")
mySQL <- (sqlStatement(groupBy = c("ScenarioID"), aggregate = c("MinimumTimestep"), where = list(MinimumTimestep = c(1))))

test_that("no errors", {
  expect_error(datasheet(myScenario), NA)
  expect_error((datasheet(myScenario, name = NULL, summary = TRUE, optional = FALSE,
                          empty = FALSE, filterColumn = NULL, filterValue = NULL,
                          lookupsAsFactors = TRUE, includeKey = FALSE, forceElements = FALSE,
                          fastQuery = FALSE)), NA)
  expect_error((datasheet(myProject, summary = FALSE)), NA)
  expect_error((datasheet(myScenario, name = "RunControl", includeKey = TRUE, summary = "TRUE", optional = TRUE)), NA)
  #expect_error((datasheet(myScenario, name = "RunControl", forceElements = TRUE, fastQuery = TRUE)), NA)
  expect_error((datasheet(myScenario, name = "RunControl", empty = TRUE, lookupsAsFactors = FALSE)), NA)
  expect_error((datasheet(myScenario, name = "RunControl", sqlStatement = mySQL)), NA)
  expect_error((datasheet(myLibrary, summary = "CORE")), NA)
  expect_error((datasheet(myLibrary, name = "core_Backup")), NA)
  expect_error((datasheet(myScenario, summary = FALSE, empty = FALSE, lookupsAsFactors = FALSE, optional = FALSE)), NA)
})

test_that("correct statements ignored", {
  expect_equal(datasheet(myScenario), (datasheet(myScenario, name = NULL, summary = TRUE, optional = FALSE,
                                                 empty = FALSE, filterColumn = NULL, filterValue = NULL,
                                                 lookupsAsFactors = TRUE, includeKey = FALSE, forceElements = FALSE,
                                                 fastQuery = FALSE)))
  expect_equal((datasheet(myScenario, name = NULL, summary = TRUE, optional = FALSE,
                          empty = FALSE, filterColumn = NULL, filterValue = NULL,
                          lookupsAsFactors = TRUE, includeKey = FALSE, forceElements = FALSE,
                          fastQuery = FALSE)), datasheet(myScenario, name = NULL, summary = TRUE, optional = FALSE,
                               empty = TRUE, filterColumn = NULL, filterValue = NULL,
                               lookupsAsFactors = FALSE, includeKey = FALSE, forceElements = TRUE,
                               fastQuery = TRUE))
  #expect_equal((datasheet(myScenario, summary = FALSE, empty = FALSE, lookupsAsFactors = FALSE, optional = FALSE)), datasheet(myScenario, summary = FALSE, empty = FALSE, lookupsAsFactors = FALSE, optional = TRUE))
})

test_that("errors work", {
  expect_error(datasheet("myScenario"))
  expect_error(datasheet(myScenario, name = c(1,2,3)))
  expect_error(datasheet(myScenario, name = RunControl))
  expect_warning(expect_error(datasheet(myScenario, name = "STSim_RunControl")), "An STSim_ prefix for a datasheet name is no longer required.")
  expect_warning(datasheet(myScenario, name = "RunControl", project = "one"), "project argument is ignored when ssimObject is a Project/Scenario or list of these.")
  expect_warning(datasheet(myScenario, name = "RunControl", scenario = "one"))
  expect_error(datasheet(myScenario, name = "RunControl", summary = "YES"))
  expect_error(datasheet(myScenario, name = "RunControl", summary = TRUE, optional = "FALSE"))
  expect_error(datasheet(myScenario, name = "RunControl", summary = FALSE, optional = FALSE, empty = "FALSE"))
  expect_error(datasheet(myScenario, name = "RunControl", summary = FALSE,
                         optional = FALSE, empty = FALSE, lookupsAsFactors = "TRUE"))
  expect_error(datasheet(myScenario, name = "RunControl", summary = FALSE,
                         optional = FALSE, empty = FALSE, lookupsAsFactors = TRUE,
                         sqlStatement = "mySQL"))
  expect_error(datasheet(myScenario, name = "RunControl", summary = FALSE,
                         optional = FALSE, empty = FALSE, lookupsAsFactors = TRUE,
                         sqlStatement = mySQL, includeKey = "FALSE"))
  expect_error(datasheet(myScenario, name = "RunControl", summary = FALSE,
                         optional = FALSE, empty = FALSE, lookupsAsFactors = TRUE,
                         sqlStatement = mySQL, includeKey = FALSE, forceElements = "FALSE"))
  expect_error(datasheet(myScenario, name = "RunControl", summary = FALSE,
                         optional = FALSE, empty = FALSE, lookupsAsFactors = TRUE,
                         sqlStatement = mySQL, includeKey = FALSE, forceElements = FALSE,
                         fastQuery = "FALSE"))
  expect_error(datasheet(name = "RunControl"))
})

