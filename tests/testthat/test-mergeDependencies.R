
test_that("can mergeDependencies all SsimObjects", {
  myLibraryName <- file.path(tempdir(), "lib")
  mySession <- session()
  myLibrary <- ssimLibrary(name = myLibraryName, session = mySession, package = "stsim")
  myProject <- project(myLibrary, project = "Definitions")
  myScenario <- scenario(myProject, scenario = "My Scenario")

  expect_error(expect_error(mergeDependencies(myScenario)))
  expect_type(mergeDependencies(myScenario) <- TRUE, "logical")
  expect_error(expect_error(mergeDependencies(myScenario) <- TRUE))
  expect_error(expect_error(mergeDependencies(myScenario)))
})

test_that("errors work", {
  myLibraryName <- file.path(tempdir(), "lib")
  mySession <- session()
  myLibrary <- ssimLibrary(name = myLibraryName, session = mySession, package = "stsim")
  myProject <- project(myLibrary, project = "Definitions")
  myScenario <- scenario(myProject, scenario = "My Scenario")

  vector <- c(1,2,3)
  list <- list(1,2,3)
  character <- "character"
  df <- data.frame(list(x = 1, y = 2, z  = 3))

  expect_error(mergeDependencies("myLibrary"))
  expect_error(mergeDependencies(vector))
  expect_error(mergeDependencies(list))
  expect_error(mergeDependencies(character))
  expect_error(mergeDependencies(df))
  expect_error(mergeDependencies(library))
  expect_error(mergeDependencies(myLibrary))
  expect_error(mergeDependencies(myProject))
  expect_error(mergeDependencies(mySession))
  expect_error(mergeDependencies(myScenario, TRUE))
  expect_error(mergeDependencies("stsim_RunControl"))
  expect_error(mergeDependencies(myScenario) <- "FALSE", "mergeDependencies must be TRUE or FALSE.")
  expect_error(mergeDependencies(myScenario) <- stsim_RunControl)
})
