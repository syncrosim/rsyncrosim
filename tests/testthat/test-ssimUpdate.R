test_that("can use ssimUpdate", {
  mySession <- session()
  myLibraryName <- file.path(tempdir(),"testlib_update")
  myLibrary <- ssimLibrary(name = myLibraryName,
                           session = mySession,
                           package = "helloworldSpatial",
                           forceUpdate = TRUE,
                           overwrite = TRUE)
  myProject <- project(myLibrary, project = "My Project")
  myScenario <- scenario(myProject, scenario = "My Scenario")

  expect_type(ssimUpdate(myScenario), "logical")
  expect_error(expect_error(ssimUpdate(myScenario)))

  expect_type(ssimUpdate(myProject), "logical")
  expect_error(expect_error(ssimUpdate(myProject)))

  expect_type(ssimUpdate(myLibrary), "logical")
  expect_error(expect_error(ssimUpdate(myLibrary)))
})

test_that("errors work", {
  mySession <- session()
  myLibraryName <- file.path(tempdir(),"testlib_update")
  myLibrary <- ssimLibrary(name = myLibraryName,
                           session = mySession,
                           package = "helloworldSpatial",
                           forceUpdate = TRUE,
                           overwrite = TRUE)
  myProject <- project(myLibrary, project = "My Project")
  myScenario <- scenario(myProject, scenario = "My Scenario")

  vector <- c(1,2,3)
  list <- list(1,2,3)
  character <- "character"
  df <- data.frame(list(x = 1, y = 2, z  = 3))

  expect_error(ssimUpdate("myScenario"))
  expect_error(ssimUpdate(vector))
  expect_error(ssimUpdate(list))
  expect_error(ssimUpdate(character))
  expect_error(ssimUpdate(df))
  expect_error(ssimUpdate(Scenario))
  expect_error(ssimUpdate(mySession))
  expect_error(ssimUpdate(NULL))
})
