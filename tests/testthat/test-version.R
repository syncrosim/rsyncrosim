test_that("version works", {
  mySession <- session()

  expect_error(version(mySession), NA)
  expect_type(version(mySession), "character")
  expect_error(version(), NA)
  expect_type(version(), "character")

})

test_that("errors work", {
  myLibraryName <- file.path(tempdir(),"testlib_sqlStatement")
  mySession <- session()
  myLibrary <- ssimLibrary(name = myLibraryName,
                           session = mySession,
                           package = "helloworldSpatial",
                           template = "example-library")
  myProject <- project(myLibrary, project = "Definitions")
  myScenario <- scenario(myProject, scenario = "My Scenario")

  expect_error(version("mySession"))
  expect_error(version(myLibrary))
  expect_error(version(myProject))
  expect_error(version(myScenario))
  expect_error(version(1))

})
