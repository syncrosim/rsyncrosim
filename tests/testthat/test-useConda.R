test_that("useConda works", {
  myLibraryName <- file.path(tempdir(),"testlib_sqlStatement")
  mySession <- session()
  myLibrary <- ssimLibrary(name = myLibraryName,
                           session = mySession,
                           package = "helloworldSpatial",
                           template = "example-library")

  expect_error(useConda(myLibrary), NA)
  expect_type(useConda(myLibrary), "logical")
  expect_equal(useConda(myLibrary), FALSE)
  expect_error(useConda(myLibrary) <- TRUE, NA)
  expect_equal(useConda(myLibrary), TRUE)
  expect_error(useConda(myLibrary) <- "FALSE", NA)
  expect_equal(useConda(myLibrary), FALSE)

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

  expect_error(useConda("myLibrary"))
  expect_error(useConda(mySession))
  expect_error(useConda(myProject))
  expect_error(useConda(myScenario))

  expect_error(useConda(myLibrary) <- NO)
  expect_error(useConda(myLibrary) <- mySession)

})
