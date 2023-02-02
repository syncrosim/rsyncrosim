## https://debruine.github.io/post/interactive-test/

#test_that("can delete all SsimObjects", {
#  myLibraryName <- file.path(tempdir(),"testlib")
#  mySession <- session()
#  myLibrary <- ssimLibrary(name = myLibraryName, session = mySession)
#  myProject <- project(myLibrary, project = "My Project")
#  myScenario <- scenario(myProject, scenario = "My Scenario")


#  expect_message(delete(myLibrary), "Do you really want to delete")

#  expect_type(delete(myLibrary), "character")
#  expect_type(delete(myProject), "NULL")
#  expect_type(delete(myScenario), "NULL")
#})

#test_that("errors work", {
#  vector <- c(1,2,3)
#  list <- list(1,2,3)
#  character <- "character"
#  df <- data.frame(list(x = 1, y = 2, z  = 3))

#  expect_error(delete(noLibrary))
#  expect_error(delete("myLibrary"))
#  expect_error(delete(vector))
#  expect_error(delete(list))
#  expect_error(delete(character))
#  expect_error(delete(df))
#  expect_error(delete(library))
#})
