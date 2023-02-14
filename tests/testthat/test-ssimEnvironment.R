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

test_that("ssimEnvironment works", {
  myLibraryName <- file.path(tempdir(),"empty")
  mySession <- session()
  myLibrary <- ssimLibrary(name = myLibraryName, session = mySession)
  myProject <- project(myLibrary, project = "Definitions")
  myScenario <- scenario(myProject, scenario = "My Scenario")

  expect_error(ssimEnvironment(myScenario))
  expect_error(ssimEnvironment(myLibrary))
  expect_error(ssimEnvironment(myProject))
  expect_error(ssimEnvironment(mySession))
  expect_error(ssimEnvironment("myScenario"))
  expect_error(ssimEnvironment(1))

})
