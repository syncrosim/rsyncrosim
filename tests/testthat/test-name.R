myLibraryName <- file.path(tempdir(), "testlib")
mySession <- session()
myLibrary <- ssimLibrary(name = myLibraryName, session = mySession, package = "stsim")
myProject <- project(myLibrary, project = "Definitions")
myScenario <- scenario(myProject, scenario = "My Scenario")


test_that("can name all SsimObjects", {
  expect_equal(name(myScenario), "My Scenario")
  expect_type(name(myScenario), "character")
  expect_error(expect_error(name(myScenario) <- "scenario"))
  expect_equal(name(myScenario), "scenario")

  expect_equal(name(myLibrary), "testlib")
  expect_type(name(myLibrary), "character")
  expect_error(expect_error(name(myLibrary) <- "Library"))
  expect_equal(name(myLibrary), "Library")

  expect_equal(name(myProject), "Definitions")
  expect_type(name(myProject), "character")
  expect_error(expect_error(name(myProject) <- "Project"))
  expect_equal(name(myProject), "Project")
})

test_that("errors work", {
  vector <- c(1,2,3)
  list <- list(1,2,3)
  character <- "character"
  df <- data.frame(list(x = 1, y = 2, z  = 3))

  expect_error(name("myLibrary"))
  expect_error(name(vector))
  expect_error(name(list))
  expect_error(name(character))
  expect_error(name(df))
  expect_error(name(library))
  expect_error(name(myScenario, "scenario"))
  expect_error(name(myScenario) <- stsim_RunControl)
})
