myLibraryName <- file.path(tempdir(),"testlib")
mySession <- session()
myLibrary <- ssimLibrary(name = myLibraryName, session = mySession, package="burnP3Plus", overwrite = TRUE)
myProject <- project(myLibrary, project = "My Project")
myScenario <- scenario(myProject, scenario = "My Scenario")


test_that("name class", {
  expect_equal(autogentags(myScenario), NULL)
  expect_type(autogentags(myScenario), "NULL")
  expect_equal((`autogentags<-`("myTag")), "myTag")
  expect_type((`autogentags<-`("myTag")), "character")
})

test_that("errors work", {
  expect_error(autogentags("myLibrary"))
  expect_error(backup(vector))
  expect_error(backup(list))
  expect_error(backup(character))
  expect_error(backup(df))
  expect_error(backup(library))
})
