test_that("can retrive runLog from Scenario", {
  mySession <- session()
  myLibraryName <- file.path(tempdir(),"library")
  myLibrary <- ssimLibrary(name = myLibraryName,
                            session = mySession,
                            package = "helloworldSpatial",
                            template = "example-library",
                            overwrite = TRUE)
  myProject <- project(myLibrary, project = "Definitions")
  myScenario <- scenario(myProject, scenario = "My Scenario")

  expect_type(runLog(myScenario), "character")
  expect_error(expect_error(runLog(myScenario)))
  result <- run(myScenario)
  expect_type(runLog(result), "character")
  expect_error(expect_equal(runLog(result), runLog(myScenario)))
})

test_that("errors work", {
  mySession <- session()
  myLibraryName <- file.path(tempdir(),"library")
  myLibrary <- ssimLibrary(name = myLibraryName,
                           session = mySession,
                           package = "helloworldSpatial",
                           template = "example-library",
                           overwrite = TRUE)
  myProject <- project(myLibrary, project = "Definitions")
  myScenario <- scenario(myProject, scenario = "My Scenario")

  vector <- c(1,2,3)
  list <- list(1,2,3)
  character <- "character"
  df <- data.frame(list(x = 1, y = 2, z  = 3))

  expect_error(runLog("myScenario"))
  expect_error(runLog(vector))
  expect_error(runLog(list))
  expect_error(runLog(character))
  expect_error(runLog(df))
  expect_error(runLog(Scenario))
  expect_error(runLog(myProject))
  expect_error(runLog(myLibrary))
  expect_error(runLog(mySession))
})
