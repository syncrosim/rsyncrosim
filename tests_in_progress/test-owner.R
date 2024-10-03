
test_that("can owner all SsimObjects", {
  myLibraryname <- file.path(tempdir(), "testlib")
  mySession <- session()
  myLibrary <- ssimLibrary(name = myLibraryname, session = mySession)
  myProject <- project(myLibrary, project = "Definitions")
  myScenario <- scenario(myProject, scenario = "My Scenario")

  expect_equal(owner(myScenario), NULL)
  expect_type(owner(myScenario), "NULL")
  expect_error(expect_error(owner(myScenario) <- "Scenario"))
  #expect_equal(owner(myScenario), "Scenario")
  #expect_type(owner(myScenario), "character")

  expect_equal(owner(myLibrary), "N/A")
  expect_type(owner(myLibrary), "character")
  expect_error(expect_error(owner(myLibrary) <- "Library"))
  expect_equal(owner(myLibrary), "Library")
  expect_type(owner(myLibrary), "character")

  expect_equal(owner(myProject), NULL)
  expect_type(owner(myProject), "NULL")
  expect_error(expect_error(owner(myProject) <- "Project"))
  #expect_equal(owner(myProject), "Project")
  #expect_type(owner(myProject), "character")
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

  expect_error(owner("myLibrary"))
  expect_error(owner(vector))
  expect_error(owner(list))
  expect_error(owner(character))
  expect_error(owner(df))
  expect_error(owner(library))
  expect_error(owner(myScenario, "scenario"))
  expect_error(owner(myScenario) <- stsim_RunControl)
})
