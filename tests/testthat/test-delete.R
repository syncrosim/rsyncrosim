## https://debruine.github.io/post/interactive-test/

test_that("can delete all SsimObjects", {
  myLibraryName <- file.path(tempdir(),"testlib")
  mySession <- session()
  myLibrary <- ssimLibrary(name = myLibraryName, session = mySession)
  myProject <- project(myLibrary, project = "My Project")
  myScenario <- scenario(myProject, scenario = "My Scenario")

  # ssimObject argument
  expect_type(delete(myScenario, force = TRUE), "logical")
  expect_type(delete(myProject, force = TRUE), "logical")
  expect_type(delete(myLibrary, force = TRUE), "logical")

  # Project argument
  myLibrary <- ssimLibrary(name = myLibraryName, session = mySession)
  myProject <- project(myLibrary, project = "My Project")
  myScenario <- scenario(myProject, scenario = "My Scenario")

  project("C:/gitprojects/rsyncrosim/tests/tempstsim.ssim", project = "To Delete")
  expect_type(delete("C:/gitprojects/rsyncrosim/tests/tempstsim.ssim", project = "To Delete", force = TRUE), "logical")
  new <- project(myLibrary, project = "New")
  ID <- projectId(new)
  expect_type(delete(myLibrary, project = c(1, ID), force = TRUE), "list")

  # Scenario argument
  myLibrary <- ssimLibrary(name = myLibraryName, session = mySession)
  myProject <- project(myLibrary, project = "My Project")
  myScenario1 <- scenario(myProject, scenario = "My Scenario 1")
  myScenario2 <- scenario(myProject, scenario = "My Scenario 2")
  sID1 <- scenarioId(myScenario1)
  sID2 <- scenarioId(myScenario2)
  myProject2 <-  project("C:/gitprojects/rsyncrosim/tests/tempstsim.ssim", project = "My Project")
  myScenario3 <- scenario(myProject2, scenario = "My Scenario 3")

 expect_type(delete(myProject, scenario = c(sID1, sID2), force = TRUE), "list")
 myScenario1 <- scenario(myProject, scenario = "My Scenario 1")
 expect_equal(delete(myLibrary, scenario = "My Scenario 1", force = TRUE), TRUE)
 expect_type(delete("C:/gitprojects/rsyncrosim/tests/tempstsim.ssim", scenario = "My Scenario 3", force = TRUE), "logical")

 # datasheet argument
 myLibrary <- ssimLibrary(name = myLibraryName, session = mySession)
 myProject <- project(myLibrary, project = "My Project")
 myScenario <- scenario(myProject, scenario = "My Scenario")

 expect_equal(delete(myScenario, datasheet = "StateClass", force = TRUE), TRUE)
 expect_type(delete(myProject, datasheet = "StateClass", force = TRUE), "logical")

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

  expect_error(delete(myLibraryName, force = TRUE))
  expect_error(delete("myLibrary"))
  expect_error(delete(vector))
  expect_error(delete(list))
  expect_error(delete(character))
  expect_error(delete(df))
  expect_error(delete(library))

  expect_error(delete(myProject, datasheet = c("Stratum", "AgeType"), force = TRUE))
  expect_error(delete(myProject, scenario = Mycenario, force = TRUE))
  expect_error(delete(myScenario, datasheet = StateClass, force = TRUE))
})
