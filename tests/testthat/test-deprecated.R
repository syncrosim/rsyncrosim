## https://debruine.github.io/post/interactive-test/

#test_that("can deprecated all SsimObjects", {
#  myLibraryName <- file.path(tempdir(),"testlib")
#  mySession <- session()
#  myLibrary <- ssimLibrary(name = myLibraryName, session = mySession)
#  myProject <- project(myLibrary, project = "My Project")
#  myScenario <- scenario(myProject, scenario = "My Scenario")


#  expect_message(deprecated(myLibrary), "Do you really want to deprecated")

#  expect_type(deprecated(myLibrary), "character")
#  expect_type(deprecated(myProject), "NULL")
#  expect_type(deprecated(myScenario), "NULL")
#})

#test_that("errors work", {
#  vector <- c(1,2,3)
#  list <- list(1,2,3)
#  character <- "character"
#  df <- data.frame(list(x = 1, y = 2, z  = 3))

#  expect_error(deprecated(noLibrary))
#  expect_error(deprecated("myLibrary"))
#  expect_error(deprecated(vector))
#  expect_error(deprecated(list))
#  expect_error(deprecated(character))
#  expect_error(deprecated(df))
#  expect_error(deprecated(library))
#})
