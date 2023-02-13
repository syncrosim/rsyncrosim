test_that("ssimEnvironment works", {
  myLibraryName <- file.path(tempdir(),"empty")
  mySession <- session()
  myLibrary <- ssimLibrary(name = myLibraryName, session = mySession)
  myProject <- project(myLibrary, project = "Definitions")
  myScenario <- scenario(myProject, scenario = "My Scenario")

  expect_error(ssimEnvironment(), NA)
  expect_type(ssimEnvironment(), "list")
  expect_s3_class(ssimEnvironment(), "data.frame")
})

test_that("runtimeInputFolder works", {
  e <- ssimEnvironment()
  e$InputDirectory <- tempdir()
  myLibraryName <- file.path(e$TempDirectory, "empty")
  myLibrary <- ssimLibrary(name = myLibraryName,
                            session = mySession,
                            package = "helloworldSpatial",
                            template = "example-library",
                            overwrite = TRUE)
  myProject <- project(myLibrary, project = "Definitions")
  myScenario <- scenario(myProject, scenario = "My Scenario")
  result <- run(myScenario)

  expect_error(runtimeInputFolder(myScenario, datasheetName = "InputDatasheet"), NA)
  expect_error(runtimeInputFolder(myScenario), NA)
  expect_error(runtimeInputFolder(result), NA)
})

test_that("runtimeOutputFolder works", {
  e <- ssimEnvironment()
  e$TempDirectory <- tempdir()
  myLibraryName <- file.path(e$TempDirectory, "empty")
  myLibrary <- ssimLibrary(name = myLibraryName,
                           session = mySession,
                           package = "helloworldSpatial",
                           template = "example-library",
                           overwrite = TRUE)
  myProject <- project(myLibrary, project = "Definitions")
  myScenario <- scenario(myProject, scenario = "My Scenario")
  result <- run(myScenario)

  expect_error(runtimeOutputFolder(myScenario, datasheetName = "InputDatasheet"), NA)
  expect_error(runtimeOutputFolder(result), NA)

})

test_that("runtimeTempFolder works", {
  e <- ssimEnvironment()
  e$TempDirectory <- tempdir()
  myLibraryName <- file.path(e$TempDirectory, "empty")
  myLibrary <- ssimLibrary(name = myLibraryName,
                           session = mySession,
                           package = "helloworldSpatial",
                           template = "example-library",
                           overwrite = TRUE)
  myProject <- project(myLibrary, project = "Definitions")
  myScenario <- scenario(myProject, scenario = "My Scenario")
  result <- run(myScenario)

  expect_error(runtimeTempFolder(tempdir()), NA)
  expect_error(runtimeTempFolder(result), NA)

})
