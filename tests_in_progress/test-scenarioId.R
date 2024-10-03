test_that("can retrive scenarioId from Scenario", {
  mySession <- session()
  myLibraryName <- file.path(tempdir(),"testlib_datasheet")
  myLibrary <- ssimLibrary(name = myLibraryName,
                           session = mySession,
                           package = "helloworldSpatial",
                           forceUpdate = TRUE,
                           overwrite = TRUE)
  myProject <- project(myLibrary, project = "Definitions")
  myScenario <- scenario(myProject, scenario = "My Scenario")

  expect_type(scenarioId(myScenario), "double")
  expect_error(expect_error(scenarioId(myScenario)))

})

test_that("errors work", {
  mySession <- session()
  myLibraryName <- file.path(tempdir(),"testlib_datasheet")
  myLibrary <- ssimLibrary(name = myLibraryName,
                           session = mySession,
                           package = "helloworldSpatial",
                           forceUpdate = TRUE,
                           overwrite = TRUE)
  myProject <- project(myLibrary, project = "Definitions")
  myScenario <- scenario(myProject, scenario = "My Scenario")

  vector <- c(1,2,3)
  list <- list(1,2,3)
  character <- "character"
  df <- data.frame(list(x = 1, y = 2, z  = 3))

  expect_error(scenarioId("myScenario"))
  expect_error(scenarioId(vector))
  expect_error(scenarioId(list))
  expect_error(scenarioId(character))
  expect_error(scenarioId(df))
  expect_error(scenarioId(Scenario))
  expect_error(scenarioId(myLibrary))
  expect_error(scenarioId(mySession))
})
