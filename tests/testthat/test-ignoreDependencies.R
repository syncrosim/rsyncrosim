
test_that("can ignoreDependencies all SsimObjects", {
  myLibraryName <- file.path(tempdir(), "testlib")
  mySession <- session()
  myLibrary <- ssimLibrary(name = myLibraryName, session = mySession, package = "stsim")
  myProject <- project(myLibrary, project = "Definitions")
  myScenario <- scenario(myProject, scenario = "My Scenario")

  expect_error(expect_error(ignoreDependencies(myScenario)))
  expect_type(ignoreDependencies(myScenario), "NULL")
  expect_error(expect_error(ignoreDependencies(myScenario) <- "stsim_RunControl,stsim_TransitionTarget"))

})

test_that("errors work", {
  myLibraryName <- file.path(tempdir(), "testlib")
  mySession <- session()
  myLibrary <- ssimLibrary(name = myLibraryName, session = mySession, package = "stsim")
  myProject <- project(myLibrary, project = "Definitions")
  myScenario <- scenario(myProject, scenario = "My Scenario")

  vector <- c(1,2,3)
  list <- list(1,2,3)
  character <- "character"
  df <- data.frame(list(x = 1, y = 2, z  = 3))

  expect_error(ignoreDependencies("myLibrary"))
  expect_error(ignoreDependencies(vector))
  expect_error(ignoreDependencies(list))
  expect_error(ignoreDependencies(character))
  expect_error(ignoreDependencies(df))
  expect_error(ignoreDependencies(library))
  expect_error(ignoreDependencies(myLibrary))
  expect_error(ignoreDependencies(myProject))
  expect_error(ignoreDependencies(mySession))
  expect_error(ignoreDependencies(myScenario, "stsim_RunControl"))
  expect_error(ignoreDependencies("stsim_RunControl"))
  expect_error(expect_error(ignoreDependencies(myScenario) <- stsim_RunControl,stsim_TransitionTarget))
  expect_error(ignoreDependencies(myScenario) <- stsim_RunControl)
})
