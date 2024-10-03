myLibraryName <- file.path(tempdir(),"testlib")
mySession <- session("C:/gitprojects/ssimbin3/")
myLibrary <- ssimLibrary(name = myLibraryName, session = mySession)
myProject <- project(myLibrary, project = "My Project")
myScenario <- scenario(myProject, scenario = "My Scenario")

expected1 <- backup(myLibrary)
expected2 <- backup(myProject)
expected3 <- backup(myScenario)

ds <- datasheet(myScenario, name = "core_Backup")
#ds <- (ds$IncludeInput = NA)

vector <- c(1,2,3)
list <- list(1,2,3)
character <- "character"
df <- data.frame(list(x = 1, y = 2, z  = 3))

test_that("can backup all SsimObjects", {
  expect_equal(expected1, TRUE)
  expect_equal(expected2, TRUE)
  expect_equal(expected3, TRUE)
  expect_type(expected1, "logical")
  expect_type(expected2, "logical")
  expect_type(expected3, "logical")
})

test_that("errors work", {
  expect_error(backup("myLibrary"))
  expect_error(backup(vector))
  expect_error(backup(list))
  expect_error(backup(character))
  expect_error(backup(df))
  expect_error(backup(library))
})
