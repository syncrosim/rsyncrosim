
test_that("can readOnly all SsimObjects", {
  myLibraryname <- file.path(tempdir(), "testlib")
  mySession <- session()
  myLibrary <- ssimLibrary(name = myLibraryname, session = mySession)
  myProject <- project(myLibrary, project = "Definitions")
  myScenario <- scenario(myProject, scenario = "My Scenario")

  expect_equal(readOnly(myScenario), FALSE)
  expect_type(readOnly(myScenario), "logical")
  expect_error(expect_error(readOnly(myScenario) <- TRUE))
  expect_equal(readOnly(myScenario), TRUE)
  expect_type(readOnly(myScenario), "logical")

  expect_equal(readOnly(myLibrary), FALSE)
  expect_type(readOnly(myLibrary), "logical")
  expect_error(expect_error(readOnly(myLibrary) <- TRUE))
  expect_equal(readOnly(myLibrary), TRUE)
  expect_type(readOnly(myLibrary), "logical")

  expect_equal(readOnly(myProject), FALSE)
  expect_type(readOnly(myProject), "logical")
  expect_error(expect_error(readOnly(myProject) <- TRUE))
  expect_equal(readOnly(myProject), TRUE)
  expect_type(readOnly(myProject), "logical")

})

test_that("errors work", {
  myLibraryname <- file.path(tempdir(), "testlib")
  mySession <- session()
  myLibrary <- ssimLibrary(name = myLibraryname, session = mySession)
  myProject <- project(myLibrary, project = "Definitions")
  myScenario <- scenario(myProject, scenario = "My Scenario")

  vector <- c(1,2,3)
  list <- list(1,2,3)
  character <- "character"
  df <- data.frame(list(x = 1, y = 2, z  = 3))

  expect_error(readOnly(mySession))
  expect_error(readOnly(vector))
  expect_error(readOnly(list))
  expect_error(readOnly(character))
  expect_error(readOnly(df))
  expect_error(readOnly(library))
  expect_error(readOnly(myScenario, TRUE))
  expect_error(readOnly(myScenario) <- "TRUE")
})
