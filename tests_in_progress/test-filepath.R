myLibraryName <- file.path(tempdir(),"testlib")
mySession <- session()
myLibrary <- ssimLibrary(name = myLibraryName, session = mySession)
myProject <- project(myLibrary, project = "My Project")
myScenario <- scenario(myProject, scenario = "My Scenario")

test_that("can filepath all SsimObjects", {
  expect_type(filepath(myLibrary), "character")
  expect_type(filepath(myProject), "character")
  expect_type(filepath(myScenario), "character")
  expect_type(filepath(mySession), "character")
})

test_that("errors work", {
  vector <- c(1,2,3)
  list <- list(1,2,3)
  character <- "character"
  df <- data.frame(list(x = 1, y = 2, z  = 3))

  expect_error(filepath("myLibrary"))
  expect_error(filepath(vector))
  expect_error(filepath(list))
  expect_error(filepath(character))
  expect_error(filepath(df))
  expect_error(filepath(library))
})
