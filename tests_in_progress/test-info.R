
test_that("can retrive info from Library", {
  myLibraryName <- file.path(tempdir(),"testlib")
  mySession <- session()
  myLibrary <- ssimLibrary(name = myLibraryName, session = mySession)
  myProject <- project(myLibrary, project = "My Project")
  myScenario <- scenario(myProject, scenario = "My Scenario")

  expect_type(info(myLibrary), "list")
  expect_s3_class(info(myLibrary), "data.frame")
  expect_error(expect_error(info(myLibrary)))
})

test_that("errors work", {
  myLibraryName <- file.path(tempdir(),"testlib")
  mySession <- session()
  myLibrary <- ssimLibrary(name = myLibraryName, session = mySession)
  myProject <- project(myLibrary, project = "My Project")
  myScenario <- scenario(myProject, scenario = "My Scenario")

  vector <- c(1,2,3)
  list <- list(1,2,3)
  character <- "character"
  df <- data.frame(list(x = 1, y = 2, z  = 3))

  expect_error(info("myLibrary"))
  expect_error(info(vector))
  expect_error(info(list))
  expect_error(info(character))
  expect_error(info(df))
  expect_error(info(library))
  expect_error(info(myProject))
  expect_error(info(myScenario))
  expect_error(info(mySession))
})
